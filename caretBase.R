mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "dataOps.R", "logger.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}


####################################################################################################################
#
# Testing Caret Classifier
#
####################################################################################################################
testCaret <- function(mva, fit, testdata, problemType, targetcol, positiveTarget, njobs) {
  require(caret)
  require(itertools)
  
  loginfo(paste("  Testing Caret",mva,"classifier."))
  loginfo(paste("    Target:",targetcol,"->",positiveTarget))
  loginfo(paste("Testing with",njobs,"jobs."))

  predValue <- NA
  predProb  <- NA
  predClass <- NA
  
  if ( is.null(fit) ) { stop(paste("Caret",mva,"fit is NULL!")) }
  
  if ( problemType == "linearRegression" ) {
    predValue <- predict(fit, testdata)
  } else if ( problemType == "binaryClassification" ) {
    predProb  <- foreach(d=isplitRows(testdata, chunks=njobs), .combine=c, .packages=c("stats")) %dopar% { predict(fit, d, type="prob")[,positiveTarget] }
    if ( mva == "gpls" ) {
      loginfo("There is a bug caret::gpls so we need to invert the probabilities")
      predProb <- 1 - predProb
    }
    predClass <- as.factor(predProb > 0.5)
    levels(predClass) <- setBinaryTargetLevels(positiveTarget, rev = T)
  } else {
    stop("Must be linearRegression or binaryClassification")
  }
  
  logdebug(paste("  Returning probabilities of length:",length(predProb)))
  logdebug(paste("  Returning class labels of length: ",length(predClass)))
  logdebug(paste("  Returning scores of length:       ",length(predValue)))
  
  return( list("value"=predValue, "prob"=predProb, "class"=predClass) )
}


####################################################################################################################
#
# Training Caret Classifier
#
####################################################################################################################
trainCaret <- function(mva, formula, traindata, targetcol, positiveTarget, maxGrid) {
  problemType <- getProblemType(traindata[,targetcol])
  loginfo(paste("Training Caret MVA:",mva,"with data of size:",getDimStr(traindata),"and target column:",targetcol,"for problem type:",problemType))
  require(caret)

  if ( problemType == "linearRegression" )     { logdebug(paste("  Training Caret",mva,"regressor.")) }
  if ( problemType == "binaryClassification" ) { logdebug(paste("  Training Caret",mva,"classifier.")) }
  
  params <- getCaretParams(mva, traindata, problemType = problemType, maxGrid = maxGrid, useGrid = F, targetcol)
  tGrid  <- params[["grid"]]
  tExtra <- params[["extra"]]
  logdebug(paste("Scaning the following parameter space using bootstrap resampling."))
  tCont  <- params[["control"]]
  if ( !is.null(tExtra) ) {
    logdebug(paste("Passing additional parameters to caret MVA",mva))
    print(tExtra)
  }
  loginfo(paste("  Running Caret",mva,"..."))
  #fit <- caret::train(form, data = traindata, method = mva, tuneGrid = tGrid, trControl = tCont)
  
  
  ##
  ## Different models need special parameters to turn off giving back the entire training dataset.
  ##
  if ( mva == "glm" | mva == "lm" ) {
    fit <- caret::train(form = formula, data = traindata, method = mva, tuneGrid = tGrid, trControl = tCont, model = F)
  } else if ( mva == "nnet" ) {
    linout = ifelse(problemType == "linearRegression", T, F)
    fit <- caret::train(form = formula, data = traindata, method = mva, tuneGrid = tGrid, trControl = tCont, MaxNWts = 1e5, maxit=100, trace = F, linout = linout)
  } else {
    fit <- caret::train(form = formula, data = traindata, method = mva, tuneGrid = tGrid, trControl = tCont)
  }
  
  fit <- trimCaretFit(fit, mva)
  
  loginfo(paste("  Running caret",mva,"... Done"))
  
  return( fit )
}


####################################################################################################################
#
# Caret Params
#
####################################################################################################################
getCaretParams <- function(mva, traindata, problemType, maxGrid, useGrid, targetcol) {
  #options    <- getEllipse(list(...))
  #print(paste("options",options))
  logdebug(paste("  Getting Caret",mva,"hyperparameters for problem",problemType))

  tGrid  <- getCaretGridParams(mva, traindata, problemType, maxGrid, useGrid, targetcol)
  extra  <- getCaretExtraParams(mva)
  params <- colnames(tGrid)
  loginfo(paste("    ",paste(tGrid, collapse = ", ")))
  #params <- as.character(tGrid[,"parameter"])
  
  trainC <- trainControl(method = "boot", number = 1, verboseIter = T, allowParallel=T, classProbs = F, returnData = F)
  if ( "none" %in% params ) {
    trainC <- trainControl(method = "none", number = 1, verboseIter = T, allowParallel=T, classProbs = F, returnData = F)
  } else {
    trainC <- trainControl(method = "boot", number = 1, verboseIter = T, allowParallel=T, classProbs = F, returnData = F)
  }
  
  logdebug(paste("  Returning grid and control for caret",mva))
  return( list("grid"=tGrid, "control"=trainC, "extra"=extra) )
}


####################################################################################################################
#
# Get Caret Params
#
####################################################################################################################
getCaretGridParams <- function(mva, traindata, problemType, maxGrid = 1, useGrid = T, targetcol = "TARGET") {
  pos <- loc(targetcol, colnames(traindata))
  if ( is.null(pos) ) { stop(paste("Could not find",targetcol,"in training data."))}
  trainX <- traindata[,-pos]
  trainY <- traindata[,pos]
  
  caretparams <- getModelInfo(model = mva, regex = F)
  if ( is.na(names(caretparams)) ) { stop(paste("No caret mva named",mva)) }
  caretparams <- caretparams[[mva]]
  if ( is.null(caretparams) ) { stop(paste("No caret mva data for mva",mva)) }
  
  params    <- NULL
  searchVal <- ifelse(useGrid == T, "grid", "random")
  if ( maxGrid == 1 ) { searchVal <- "grid" }
  
  result = tryCatch({
    classgrid <- caretparams[["grid"]]
    params    <- classgrid(x = trainX, y = trainY, len = 1, search = "grid")
    for ( lval in seq(3, 9, by = 2) ) {
      params <- unique(rbind(params, classgrid(x = trainX, y = trainY, len = lval, search = searchVal)))
      if ( nrow(params) >= maxGrid ) { break }
    }
    if ( nrow(params) > maxGrid ) {
      params <- head(params, n=maxGrid)
    }
  })
  
  params <- getSpecialCaretParams(mva = mva, params = params, ncols = ncol(trainX), problemType = problemType)

  return( params )
}


####################################################################################################################
#
# Get Special Caret Params
#
####################################################################################################################
getSpecialCaretParams <- function(mva, params, ncols, problemType) {
  for ( col in colnames(params) ) {
    oldParams <- params[,col]
    newParams <- NULL
    if ( col == "K.prov" & ncols >= 30 ) {
      newParams <- var_seq(p = max(ncols/3, 20), classification = T, len = nrow(params))
    }
    
    if ( !(is.null(newParams)) ) {
      loginfo(paste("Replacing grid parameter",col))
      loginfo(paste("Old:",paste(params[,col], collapse = ", ")))
      params[,col] <- newParams
      loginfo(paste("New:",paste(params[,col], collapse = ", ")))
    }
  }
  
  return( params )
}


####################################################################################################################
#
# Get Special Caret Params
#
####################################################################################################################
getCaretExtraParams <- function(mva) {
  extra <- NULL
  #if ( mva == "blasso" ) { extra <- "T=10 "}
  return( extra )
}




####################################################################################################################
#
# Available Classifiers
#
####################################################################################################################
getCaretClassifiers <- function() {
  load(file=file.path(mvadir, "caret", "caretclassifiers.rData")) # caretclassifiers
  writeLines("Remove bagFDAGCV because of grid tuning issues.")
  caretclassifiers <- rmElement("bagFDAGCV", caretclassifiers)
  writeLines("Remove spls because it takes forever.")
  caretclassifiers <- rmElement("spls", caretclassifiers)
  writeLines("Remove bagFDA because it takes forever.")
  caretclassifiers <- rmElement("bagFDA", caretclassifiers)
  writeLines("Remove bayesglm because of grid tuning issues.")
  caretclassifiers <- rmElement("bayesglm", caretclassifiers)
  writeLines("Remove loclda because of grid tuning issues.")
  caretclassifiers <- rmElement("loclda", caretclassifiers)
  writeLines("Remove mlp* because it doesn't seem any good.")
  caretclassifiers <- rmElement("mlp", caretclassifiers)
  caretclassifiers <- rmElement("mlpML", caretclassifiers)
  caretclassifiers <- rmElement("mlpWeightDecay", caretclassifiers)
  caretclassifiers <- rmElement("mlpWeightDecayML", caretclassifiers)
  writeLines("Remove fda because it can't earth?")
  caretclassifiers <- rmElement("fda", caretclassifiers)
  
  
  undefCols <- c("hdda", "svmLinear2", "svmLinearWeights")
  for ( classifier in undefCols ) {
    writeLines(paste("Removing",classifier,"due to undefined columns selected error."))
    caretclassifiers <- rmElement(classifier, caretclassifiers) # just classes
  }
  
  justClasses <- c("BstLm", "bstTree", "CSimca", "deepboost", "ownn", "partDSA", "protoclass", "rpartCost")
  justClasses <- c(justClasses, "RSimca", "snn", "rFerns", "elm", "lssvmRadial", "lvq", "svmLinear3")
  justClasses <- c(justClasses, "rpartScore", "svmLinearWeights2", "C5.0Cost", "Mlda", "PenalizedLDA")
  justClasses <- c(justClasses, "RFlda", "rocc")
  
  for ( classifier in justClasses ) {
    writeLines(paste("Removing",classifier,"because it don't allow probabilities."))
    caretclassifiers <- rmElement(classifier, caretclassifiers) # just classes
  }
  
  writeLines(paste("Removing nb because testing takes forever (no idea why?)"))
  caretclassifiers <- rmElement("nb", caretclassifiers)
  require(glmnet)
  require(sdwd)
  require(pamr)
  
  return( caretclassifiers )
}

####################################################################################################################
#
# Available Regressors
#
####################################################################################################################
getCaretBasicRegressors <- function() {
  return( c("nnet") )
  
  return( c("lm", "glm", "nnet", "rf", "xgbTree") )
}
getCaretRegressors <- function() {
  load(file=file.path(mvadir, "caret", "caretregressors.rData")) # caretregressors

  
  ## NAs
  NAerrors <- c("bag", "bstSm", "foba", "gamboost", "GFS.THRIFT", "logicBag", "logreg", "neuralnet", "rvmLinear", "rvmPoly", "SBC", "xyf")
  NAerrors <- c(NAerrors, "bam", "gam", "blackboost", "gamLoess", "glm.nb", "parRF", "mlpWeightDecayML")
  for ( mva in NAerrors ) {
    writeLines(paste("Removing",mva,"because it produces NAs"))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## Slow no output…
  slows <- c("ANFIS", "FIR.DM", "FS.HGD", "GFS.LT.RS", "HYFIS", "WM", "DENFIS")
  for ( mva in slows ) {
    writeLines(paste("Removing",mva,"because it doesn't output anything..."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  badfits <- c("GFS.FR.MOGUL", "krlsPoly", "mlp", "mlpML", "mlpSGD", "mlpWeightDecay")
  for ( mva in badfits ) {
    writeLines(paste("Removing",mva,"because they seem to give terrible fits."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  
  ## Works", "but super slow…
  tooslows <- c("rfRules", "nodeHarvest", "monmlp")
  for ( mva in tooslows ) {
    writeLines(paste("Removing",mva,"because it is super slow."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## No H20
  h2os <- c("gbm_h2o", "glmnet_h2o")
  for ( mva in h2os ) {
    writeLines(paste("Removing",mva,"because I need H2O installed."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## RWeka
  rwekas <- c("M5", "M5Rules")
  for ( mva in rwekas ) {
    writeLines(paste("Removing",mva,"because I need RWeka installed."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## Odd error
  odds <- c("ordinalNet", "randomGLM")
  for ( mva in odds ) {
    writeLines(paste("Removing",mva,"because it gives an odd error."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## Python dependency
  pythons <- c("pythonKnnReg")
  for ( mva in pythons ) {
    writeLines(paste("Removing",mva,"because it require rPython and other python stuff."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## No package
  extincts <- c("relaxo")
  for ( mva in extincts ) {
    writeLines(paste("Removing",mva,"because the packages doesn't exist anymore."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  ## String MVA
  strings <- c("svmBoundrangeString", "svmExpoString", "svmSpectrumString")
  for ( mva in strings ) {
    writeLines(paste("Removing",mva,"because it is a text MVA."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  require(rpart)
  require(earth)
  require(glmnet)
  
  required <- c("blasso", "blassoAveraged", "bridge")
  for ( mva in required ) {
    writeLines(paste("Removing",mva,"because I need to understand the internal parameters."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  barts <- c("bartMachine", "extraTrees")
  for ( mva in barts ) {
    writeLines(paste("Removing",mva,"because I cannot store the result and reload."))
    caretregressors <- rmElement(mva, caretregressors)
  }
  
  return( caretregressors )
  
  # ANFIS (Slow)
}


####################################################################################################################
#
# Trim Fit Information
#
####################################################################################################################
trimCaretFit <- function(fit, mva) {
  loginfo(paste("  Trimming Caret Fit for MVA:",mva))
  
  print(showMem(fit))
  if ( mva == "glm" | mva == "lm" ) {
    fit[["finalModel"]] <- stripGlmLR(fit[["finalModel"]])
  }
  print(showMem(fit))
  return( fit )
}

stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}