source("mva/mlrBase.R")

getMLRParams <- function(params) {
  cols  <- c("id", "type", "default", "lower", "upper", "tunable")
  mlrdata <- list()
  for ( classifier in names(params) ) {
    pdata <- params[[classifier]]
    vars  <- names(pdata)
    cdata <- list()
    for ( var in vars ) {
      vdata <- pdata[[var]]
      vardata <- list()
      for ( col in cols ) {
        tmp <- vdata[[col]]
        print(paste(var,col,tmp,is.null(tmp)))
        if (is.null(tmp)) { value <- NA }
        else              { value <- vdata[[col]] }
        vardata[[col]] <- value
        print(paste(var,col,vardata[[col]]))
      }
      cdata[[var]] <- unlist(vardata)
    }
    cdata <- as.data.frame(t(data.frame(cdata)))
    rownames(cdata) <- NULL
    mlrdata[[classifier]] <- cdata
  }
  save(mlrdata, file="mva/mlr/mlrdata.rData")
}

analyzeFits <- function() {
  load(file="mva/mlr/fits.rData") # fits
  load(file = "mva/mlr/params.rData") # mlrparams

  mlrClassifierParams <- list()
  for ( classifier in names(fits) ) {
    model <- fits[[classifier]][["learner.model"]]
    modelvals <- names(model)
    mlrmodelvals <- names(mlrparams[[classifier]])
    
    lrn  <- makeLearner(classifier, predict.type = "prob")    
    hyper <- names(getHyperPars(lrn))
    
    keep <- intersect(modelvals, mlrmodelvals)
    keep <- unique(c(keep, hyper))
    writeLines("\n\n")
    writeLines(classifier)
    print(keep)
  }
}

fitMLR <- function(classifier) {
  fits <- list()
  posTar <- "yes"
  target <- "TARGET"
  for ( classifier in classifiers ) {
    writeLines("\n\n")
    writeLines(classifier)
    if ( classifier %in% c("classif.bdk", "classif.bst", "classif.knn", "classif.LiblineaRL1L2SVC", 
                           "classif.LiblineaRL2L1SVC", "classif.LiblineaRL2SVC", "classif.LiblineaRMultiClassSVC",
                           "classif.lssvm", "classif.lvq1", "classif.qda", "classif.rFerns",
                           "classif.rrlda", "classif.xyf") ) { next }
    result = tryCatch({
      lrn  <- makeLearner(classifier, predict.type = "prob")
      task <- makeClassifTask(data = traindata, target = target, positive = posTar)
      fits[[classifier]] <- mlr::train(learner = lrn, task = task)  
    })
    print(names(fits))
  }
  save(fits, file="mva/mlr/fits.rData", compress = T)
}

getParams <- function(classifiers) {
  require(mlr)
  mlrparams <- list()
  for ( classifier in classifiers ) {
    print(classifier)
    mlrparams[[classifier]]
    vals <- getParamSet(classifier)[["pars"]]
    for ( varname in names(vals) ) {
      tunable <- vals[[varname]][["tunable"]]
      vtype   <- vals[[varname]][["type"]]
      minval  <- vals[[varname]][["lower"]]
      maxval  <- vals[[varname]][["upper"]]
      default <- vals[[varname]][["default"]]
      if ( tunable ) {
        if ( vtype == "logical" )       { next }
        if ( vtype == "discrete" )      { next }
        if ( vtype == "numericvector" ) { next }
        mlrparams[[classifier]][[varname]] <- vals[[varname]]
        print(paste(varname,vtype,minval,maxval,default, sep = ", "))
      }
    }
    writeLines("\n\n")
  }
  
  save(mlrparams, file = "mva/mlr/params.rData", compress = T)
}

f()

problemType <- getProblemType(truthdata)
for ( classifer in mlrs ) {
  name <- gsub("classif.", "", classifer)
  fit  <- trainMLR(mva = name, formula = formula, traindata = traindata, target = target, positiveTarget = positiveTarget,
                   c(allowedClassifers, mvaopts, targetType, debug))
  prediction  <- testMLR(mva = name, fit = fit, testdata = testdata, problemType = problemType,
                    target = target, positiveTarget = positiveTarget, c(mvaopts, debug, targetType))
  performance <- performanceMVA(prediction, truthdata, "TARGET", debug = T)  
}

classifiers <- listLearners("classif")[,"class"]
fits <- list()
target <- "TARGET"
posTar <- "setosa"
for ( classif in classifiers ) {
  result = tryCatch({
    lrnname <- classif
    print(lrnname)
    lrn  <- makeLearner(lrnname, predict.type = "prob")
    task <- makeClassifTask(data = traindata, target = target, positive = posTar)
    fits[[lrnname]] <- mlr::train(learner = lrn, task = task)
  }, warning = function(w) {
    print(paste("MY_WARNING:  ",w))
  }, error = function(e) {
    print(paste("MY_ERROR:  ",e))
  })
}
