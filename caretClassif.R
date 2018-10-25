if ( dir.exists("/home/af02717") ) { setwd("/home/af02717")}
if ( dir.exists("/Users/tgadfort/Documents") ) { setwd("/Users/tgadfort/Documents")}

source("mva/runner.R")
source("mva/caretBase.R")
problemType <- getProblemType(truthdata)
source("mva/caretParams.R")


getMtry <- function(nvars, problemType) {
  require(caret)
  if ( problemType == "binaryClassification") {
    mtries <- var_seq(p = nvars, classification = T, len = 5)
    mtries < tail(x = head(x = mtries, n=4), n=3)
  }
  if ( problemType == "linearyRegression") {
    mtries <- var_seq(p = nvars, classification = F, len = 5)
    mtries < tail(x = head(x = mtries, n=4), n=3)
  }
  return( mtries )
}

saveCaretYaml <- function(ydata, filename = file.path(mvadir, "caretParams.yaml")) {
  require(yaml)
  writeLines(paste("Saving data to",filename))
  writeLines(as.yaml(ydata, column.major = F),con = filename)
}

getCaretYaml <- function(filename = file.path(mvadir, "caretParams.yaml")) {
  caretdata <- yaml::yaml.load_file(filename)
  # clean it up
  for ( classifier in names(caretdata) ) {
    tmp  <- caretdata[[classifier]]
    npar <- length(tmp)
    caretdata[[classifier]] <- data.frame(matrix(unlist(tmp), nrow = npar, byrow=T))
    colnames(caretdata[[classifier]]) <- c("parameter", "class", "label", "default", "min", "max", "imp")
  }
  return( caretdata )
}


#fits <- list()
warns <- list()
target <- "TARGET"
posTar <- "setosa"
#traindata <- traindata[,-c(1,2)]
pos <- loc(target, colnames(traindata))
formula <- as.formula(paste(colnames(traindata)[pos],"~", paste(colnames(traindata)[-pos], collapse= "+")))
load(file = file.path(mvadir, "caret", "good.rData"))
g1 <- head(good, n=37)
g2 <- tail(head(good, n=74), n = 37)
g3 <- tail(good, n=length(good)-74)


createCaretData <- function() {
  load(file = file.path(mvadir, "caret", "good.rData"))
  caretdata <- list()
  carets    <- getModelInfo()
  for ( classifier in good ) {
    print(classifier)
    caretdata[[classifier]] <- as.data.frame(carets[[classifier]][["parameters"]])
    if (nrow(caretdata[[classifier]]) == 0 ) {
      caretdata[[classifier]] <- data.frame("parameter"="none", "class"="none", "label"="none")
    }
    print(caretdata[[classifier]])
    caretdata[[classifier]][,"default"] <- NA
    caretdata[[classifier]][,"min"] <- NA
    caretdata[[classifier]][,"max"] <- NA
    caretdata[[classifier]][,"imp"] <- NA
  }
  save(caretdata, file = "caretdata.rData")
}

getBestTunes <- function(good = NULL, over = F) {
  require(caret)
  if ( is.null(good) ) { 
    load(file = file.path(mvadir, "caret", "good.rData"))
  }
  #caretdata <- getCaretYaml()
  load(file = "caretdata.rData")
  writeLines(paste("Found",length(good),"good classifiers"))
  if ( over ) { writeLines("Overwritting previous data.") }
  fits <- list()
  warnings <- c()
  errors   <- c()
  for ( classifier in good ) {
    caretfits <- caretdata[[classifier]]
    known <- !(any(is.na(caretfits[,"default"])))
    if ( !over & known ) { next }
    if ( any(c("mtry", "predFixed", "xgenes") %in% caretfits[,"parameter"]) ) { next }
    writeLines(paste("\n\nTrying",classifier))
    print(caretfits)
    tuneC = trainControl(method = "cv", number = 2, verboseIter = F, allowParallel=T, classProbs = F)    
    result = tryCatch({
      fits[[classifier]] <- caret::train(formula, data=traindata, tuneLength = 3, method=classifier, trControl=tuneC)
    }, error = function(e) {
      print(paste("MY_ERROR:  ",e))
    })
    tune <- fits[[classifier]][["bestTune"]]
    print("===> Bets Tune <====")
    print(tune)
    print(paste("Good:",paste(names(fits), collapse = ",")))
    print(paste("Warn:",paste(warnings, collapse = ",")))
    print(paste(" Err:",paste(errors, collapse = ",")))
    print(paste(" Len:",length(search())))
    writeLines("\n\n")
    if ( length(search()) > 95 ) { break }

#    , warning = function(w) {
#      print(paste("MY_WARNING:  ",w))
#      warnings <- c(warnings, classifier)
#    }    
  }
  return( fits )
}

getCaretGrid <- function(carets) {
  caretgrid <- list()
  pos <- loc("TARGET", colnames(traindata))
  y <- traindata[,pos]
  x <- traindata[,-pos]
  for ( classifier in names(carets) ) {
    info <- carets[[classifier]]
    result = tryCatch({
      gridval <- info$grid(x = x, y = y, len=3)
    }, error = function(e) {
      print(paste("MY_ERROR:  ",e))
      gridval <- NULL
    })
    if ( is.null(gridval) ) { next }
    print(gridval)
    caretgrid[[classifier]] <- gridval
  }
  
  save(caretgrid, file="caretgrid3.rData", compress = T)
}


mergeFitTunes <- function(fits) {
  load(file = "caretdata.rData")
  #caretdata[["hdda"]][,"class"] <- c("numeric", "character")
  for ( classifier in names(caretdata) ) {
    if ( is.null(fits[[classifier]]) ) { next }
    writeLines("\n\n")
    print(classifier)
    tune <- fits[[classifier]][["bestTune"]]
    #print(caretdata[[classifier]])
    for ( i in seq(nrow(caretdata[[classifier]])) ) {
      param <- as.character(caretdata[[classifier]][i,"parameter"])
      ctype <- as.character(caretdata[[classifier]][i,"class"])
      value <- tune[,param]
      print(tune)
      print(colnames(tune))
      if ( classifier == "kknn") {
        print(tune[,"kmax"])
        pos <- loc(param, colnames(tune))
        print(typeof(param))
        print(class(param))
        print(param)
        print(pos)
        print(tune[,param])
        print(tune[,pos])
      }
      print(paste(i,"  ",param,"  ",ctype,"---->",value))
      if ( ctype == "numeric") {
        caretdata[[classifier]][i,"default"] <- value
        if ( is.na(caretdata[[classifier]][i,"min"]) ) { caretdata[[classifier]][i,"min"] <- value }
        if ( is.na(caretdata[[classifier]][i,"max"]) ) { caretdata[[classifier]][i,"max"] <- value }
        caretdata[[classifier]][i,"min"] <- min(value, caretdata[[classifier]][i,"min"])
        caretdata[[classifier]][i,"max"] <- max(value, caretdata[[classifier]][i,"max"])
      } else if ( ctype == "character") {
        caretdata[[classifier]][i,"default"] <- as.character(value)
        if ( is.na(caretdata[[classifier]][i,"min"]) ) { caretdata[[classifier]][i,"min"] <- as.character(value) }
        if ( is.na(caretdata[[classifier]][i,"max"]) ) { caretdata[[classifier]][i,"max"] <- as.character(value) }
        if ( as.character(value) != as.character(caretdata[[classifier]][i,"min"]) ) {
          caretdata[[classifier]][i,"min"] <- as.character(value)
        } else if ( as.character(value) != as.character(caretdata[[classifier]][i,"max"]) ) {
          caretdata[[classifier]][i,"max"] <- as.character(value)
        }
      } else if ( ctype == "logical") {
        caretdata[[classifier]][i,"default"] <- as.logical(value)
        if ( is.na(caretdata[[classifier]][i,"min"]) ) { caretdata[[classifier]][i,"min"] <- as.logical(value) }
        if ( is.na(caretdata[[classifier]][i,"max"]) ) { caretdata[[classifier]][i,"max"] <- as.logical(value) }
        if ( as.logical(value) != as.logical(caretdata[[classifier]][i,"min"]) ) {
          caretdata[[classifier]][i,"min"] <- as.logical(value)
        }
      } else {
        print(ctype)
        value <- tune[,param]
        print(paste(i,"  ",param,"---->",value))
        g()
      }
    }
    print(caretdata[[classifier]])
  }
  writeLines("Saving caretdata")
  save(caretdata, file = "caretdata.rData")
}


bestTune <- lapply(fits, function(x) x[["bestTune"]])
results <- lapply(fits, function(x) x[["results"]])
#save(bestTune, results, file="fits_iris1.rData")
f()



carets <- c()
for ( classifier in carets ) {
  name <- gsub("classif.", "", classifier)
  fit  <- trainMLR(mva = name, formula = formula, traindata = traindata, target = target, positiveTarget = positiveTarget,
                   c(allowedClassifiers, mvaopts, targetType, debug))
  prediction  <- testMLR(mva = name, fit = fit, testdata = testdata, problemType = problemType,
                         target = target, positiveTarget = positiveTarget, c(mvaopts, debug, targetType))
  performance <- performanceMVA(prediction, truthdata, "TARGET", debug = T)  
}

caretclassifiers <- getModelInfo()
caretparams <- sapply(caretclassifiers, function(x) nrow(x[["parameters"]]))

#require(doMC)
#registerDoMC(cores = 3)

if ( is.null(good) ) { 
  load(file = file.path(mvadir, "caret", "good.rData"))
}
carets <- c()
skips  <- c("awtan", "awnb", "chaid", "dda", "manb", "nbDiscrete", "nbSearch", 
            "gaussprLinear", "hdrda", "J48", "JRip", "LMT", "oblique.tree", "OneR",
            "PART", "plsRglm", "polr", "rda", "rbf", "rbfDDA", "tan", "tanSearch", "rlda",
            "bag", "binda", "Linda", "lssvmLinear", "QdaCov", "vbmpRadial",
            "bam", "FRBCS.CHI", "FRBCS.W", "gam", "gamLoess", "glmnet_h2o",
            "logicBag", "logreg", "rmda", "svmBoundrangeString", "svmExpoString",
            "svmSpectrumString", "vglmAdjCat", "vglmContRatio", "vglmCumulative",
            "GFS.GCCL", "hda", "lssvmPoly", "rrlda", "SLAVE", "smda")
longs  <- c("AdaBoost.M1", "ordinalNet", "randomGLM")
for ( classifier in names(caretclassifiers) ) {
  if ( classifier %in% skips ) { next }
  if ( classifier %in% good ) { next }
  if ( classifier %in% longs ) { next }
  if ( "Classification" %in% caretclassifiers[[classifier]][["type"]] ) {
     carets <- c(carets, classifier)
  }
}




torun <- carets[sapply(carets, function(x) ifelse(caretparams[x] > 4, F, T))]
f()

for ( classifier in torun ) {
  print(paste("\nTrying",classifier))
  #if ( classif == "plr") { break }
  #if ( classif == "pls") { break }
  #if ( classif == "plsRglm") { break }
  #if ( classif == "polr") { break }
  #if ( classif == "ppr") { break }
  tuneC = trainControl(method = "cv", number = 2, verboseIter = F, allowParallel=T, classProbs = F)    
    result = tryCatch({
    fits[[classif]] <- caret::train(formula, data=traindata, tuneLength = 1, method=classifier, trControl=tuneC)
  }, warning = function(w) {
    print(paste("MY_WARNING:  ",w))
    warns[[classif]] <- classifier
  }, error = function(e) {
    print(paste("MY_ERROR:  ",e))
  })
  #print(fits)
  good <- names(fits)
  #save(good, file = "good.rData")
  print(good)
  #print(names(warns))

  #print(fits[[classif]])
}

#print(names(warns))
