mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "logger.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}


####################################################################################################################
#
# Testing MLR Classifier
#
####################################################################################################################
testMLR <- function(mva, fit, testdata, problemType, targetcol, positiveTarget, njobs) {
  require(mlr)
  require(itertools)
  
  loginfo(paste("  Testing MLR",mva,"mva with",njobs,"jobs and expecting",nrow(testdata),"results for problem:",problemType))
  
  predValue <- NA
  predProb  <- NA
  predClass <- NA
  
  if ( is.null(fit) ) {
    logwarn(paste("MLR",mva,"fit is NULL so we can not test the fit. Returning NULL"))
    return( NULL )
  }
  
  if ( problemType == "linearRegression" ) {
    #pred      <- foreach(d=isplitRows(testdata, chunks=njobs), .combine=c, .packages=c("stats")) %dopar% { predict(fit, newdata=d) }
    pred      <- predict(fit, newdata=testdata)
    preddata  <- pred[["data"]]
    predValue <- preddata[,"response"]
  } else if ( problemType == "binaryClassification" ) {
    logdebug(paste("  Getting MLR test results for",problemType))
    pred      <- foreach(d=isplitRows(testdata, chunks=njobs), .combine=c, .packages=c("stats")) %dopar% { predict(fit, newdata=d) }
    preddata  <- pred[["data"]]
    col       <- paste("prob",positiveTarget,sep=".")
    predProb  <- preddata[,col]
    predClass <- preddata[,"response"]
  } else if ( problemType == "cluster" ) {
    logdebug(paste("  Getting MLR test results for",problemType))
    pred      <- predict(fit, newdata=testdata)
    preddata  <- pred[["data"]]
    predValue <- preddata[,"response"]
  } else {
    stop("Must be linearRegression, cluster, or binaryClassification")
  }
  
  logdebug(paste("  Returning probabilities of length:",length(predProb)))
  logdebug(paste("  Returning class labels of length: ",length(predClass)))
  logdebug(paste("  Returning scores of length:       ",length(predValue)))
  return( list("value"=predValue, "prob"=predProb, "class"=predClass) )
}


####################################################################################################################
#
# Training MLR Classifier
#
####################################################################################################################
trainMLR <- function(mva, formula, traindata, target, positiveTarget, maxGrid, paramLVL) {
  problemType <- getProblemType(traindata[,targetcol])
  
  loginfo(paste("  Training MLR MVA:",mva,"with data of size:",getDimStr(traindata),"and target column:",targetcol,"for problem type:",problemType))
  require(mlr)
  
  if ( problemType == "linearRegression" )     { logdebug(paste("  Training MLR",mva,"regressor.")) }
  if ( problemType == "binaryClassification" ) { logdebug(paste("  Training MLR",mva,"classifier.")) }
  if ( problemType == "cluster" )              { logdebug(paste("  Training MLR",mva,"cluster.")) }
  
  
  if ( problemType == "linearRegression" )     { lrnname <- paste("regr",mva,sep=".") }
  if ( problemType == "binaryClassification" ) { lrnname <- paste("classif",mva,sep=".") }
  if ( problemType == "cluster" )              { lrnname <- paste("cluster",mva,sep=".") }
  
  
  logdebug(paste("Creating MLR Task for",lrnname))
  if ( problemType == "linearRegression" )     { 
    task <- tryCatch( makeRegrTask(data = traindata, target = target), error = function(e) { print(e); NULL } )
  }
  if ( problemType == "binaryClassification" ) {
    task <- tryCatch( makeClassifTask(data = traindata, target = target, positive = positiveTarget), error = function(e) { print(e); NULL } )
  }
  if ( problemType == "cluster" ) {
    task <- tryCatch( makeClusterTask(data = traindata), error = function(e) { print(e); NULL } )
  }
  if ( is.null(task) ) {
    logwarn("There was an error when creating the MLR task. Setting fit to NULL and returning.")
    return( list("fit"=NULL, "imp"=NULL) )
  }
  

  logdebug("Creating MLR feature importance")
  imp <- tryCatch( generateFilterValuesData(task, method = c("information.gain", "chi.squared")), error = function(e) { print(e); NULL } )
  
  
  logdebug("Creating MLR Learner")
  lrn <- tryCatch( createMLRLearner(lrnname, traindata, problemType, positiveTarget, task, maxGrid, paramLVL), error = function(e) { print(e); NULL } )
  if ( is.null(lrn) ) {
    logwarn("There was an error when creating the MLR learner. Setting fit to NULL and returning.")
    return( list("fit"=NULL, "imp"=imp) )
  }
  
  loginfo(paste("  Running MLR",mva,"..."))
  fit <- tryCatch( mlr::train(learner = lrn, task = task), error = function(e) { print(e); NULL } )
  
  if ( is.null(fit) ) { loginfo(paste("  Running MLR",mva,"... Done. There was an error and fit is NULL.")) }
  else                { loginfo(paste("  Running MLR",mva,"... Done. Fit is good.")) }
  
  fit <- trimMLRFit(fit, mva)
  fit[["imp"]] <- imp
  
  return( fit )
}


####################################################################################################################
#
# Available Classifiers/Regressors/Clusters
#
####################################################################################################################
getMLRClusters <- function(lvl = 1, nrows = NULL, ncols = NULL) {
  require(clue)
  
  if ( is.null(nrows) & is.null(ncols) ) {
    traindata <- get0("traindata")
    if ( is.null(traindata) ) {
      nrows <- 100
      ncols <- 2
    } else {
      nrows <- nrow(traindata)
      ncols <- ncol(traindata)
    }
  }
  
  loginfo(paste("Getting MLR clusters for nrows x cols =",nrows,"x",ncols))
  
  lrns = listLearners("cluster")
  mlrclusters <- lrns[["class"]]
  clusters <- sapply(strsplit(x = mlrclusters, split = ".", fixed = T), function(x) tail(x, n=1))
  
  #if ( lvl == 1 ) { return( c("glmnet", "xgboost", "ranger", "ctree", "earth") ) }
  return( clusters )
}

getMLRRegressors <- function(lvl = 1, nrows = NULL, ncols = NULL) {
  require(mlr)
  require(party)
  require(partykit)
  require(randomForest)
  require(randomForestSRC)
  require(glmnet)
  require(gbm)
  require(xgboost)
  require(e1071)
  require(earth)
  require(RSNNS)
  require(nnet)
  require(rknn)
  require(FNN)
  require(flare)
  require(tgp)
  require(GPfit)
  require(DiceKriging)
  require(rsm)
  require(laGP)
  require(elmNN)
  #require(ranger)
  
  if ( is.null(nrows) & is.null(ncols) ) {
    traindata <- get0("traindata")
    if ( is.null(traindata) ) {
      nrows <- 100
      ncols <- 2
    } else {
      nrows <- nrow(traindata)
      ncols <- ncol(traindata)
    }
  }
  
  loginfo(paste("Getting MLR regressors for nrows x cols =",nrows,"x",ncols))
  
  lrns = listLearners("regr")
  mlrregressors <- lrns[["class"]]
  regressors <- sapply(strsplit(x = mlrregressors, split = ".", fixed = T), function(x) tail(x, n=1))
  
  regs <- c("featureless", "bst", "LiblineaRL2L1SVR", "LiblineaRL2L2SVR", "rvm", "elmNN")
  for ( regressor in regs ) {
    logdebug(paste("  Removing",regressor,"because: it gives a really weird fit. Work it out later."))
    regressors <- rmElement(regressor, regressors)
  }
  
  regs <- c("bdk", "deeplearning", "fusedlasso")
  for ( regressor in regs ) {
    logdebug(paste("  Removing",regressor,"because: it does not work for MLR."))
    regressors <- rmElement(regressor, regressors)
  }
  
  regressor <- "xyf"
  logdebug(paste("  Removing",regressor,"because: Error in !toroidal : invalid argument type."))
  regressors <- rmElement(regressor, regressors)
  
  regressor <- "extraTrees"  
  regressors <- rmElement(regressor, regressors)
  logdebug(paste("  Remving",regressor,"because: Assertion on 'y' failed: Must be of type 'numeric', not 'NULL'."))
  
  regressor <- "frbs"  
  regressors <- rmElement(regressor, regressors)
  logdebug(paste("  Remving",regressor,"because: Error in range.data[1, j] : incorrect number of dimensions."))
  
  
  if ( "ranger" %in% rownames(installed.packages()) ) {
    regs <- c("randomForest", "RRF")
    for ( regressor in regs ) {
      regressors <- rmElement(regressor, regressors)
      logdebug(paste("  Removing",regressor,"because: We should just use ranger instead."))
    }
  } else {
    regs <- c("ranger")
    for ( regressor in regs ) {
      regressors <- rmElement(regressor, regressors)
      logdebug(paste("  Removing",regressor,"because: Ranger is not installed."))
    }
  }
  
  regs <- c("lasso", "ridge")
  for ( regressor in regs ) {
    regressors <- rmElement(regressor, regressors)
    logdebug(paste("  Remving",regressor,"because: S3 method 'makeRLearner.regr.lasso' not found."))
  }
  
  regressor <- "bartMachine"
  regressors <- rmElement(regressor, regressors)
  logdebug(paste("  Remving",regressor,"because: I need to learn how to serialize it."))
  
  regs <- "rknn, laGP"
  for ( regressor in regs ) {
    regressors <- rmElement(regressor, regressors)
    logdebug(paste("  Remving",regressor,"because: There is a problem with testing it."))
  }
  
  if ( ncols <= 2 ) {
    regs <- c("glmnet", "cvglmnet")
    for ( regressor in regs ) {
      regressors <- rmElement(regressor, regressors)
      logdebug(paste("  Remving",regressor,"because: x should be a matrix with 2 or more columns."))
    }
  }
  
  if ( ncols > 100 ) {
    regs <- c("gamboost")
    for ( regressor in regs ) {
      regressors <- rmElement(regressor, regressors)
      logdebug(paste("  Remving",regressor,"because: it can't handle large degrees of freedom."))
    }
  }
  
  if ( nrows <= 100 ) {
    regs <- c("gbm", "ranger")
    for ( regressor in regs ) {
      regressors <- rmElement(regressor, regressors)
      logdebug(paste("  Remving",regressor,"because: it just needs more data."))
    }
  }
  
  if ( lvl == 1 ) { 
    if ( "ranger" %in% rownames(installed.packages()) ) {
      return( c("glmnet", "xgboost", "ranger", "ctree", "earth") )
    } else {
      return( c("glmnet", "xgboost", "randomForest", "ctree", "earth") )
    }
  }
  if ( lvl == 2 ) { 
    regressors <- regressors[!(regressors %in% c("evtree", "randomForestSRC", "GPfit", "laGP", "km", "bgp", "btgp", "bgpllm", "btgpllm", "btlm", "blm", "gausspr"))]
    regressors <- c("randomForest", regressors)
  }
  return( regressors )
}

getMLRClassifiers <- function(lvl = 1, nrows = NULL, ncols = NULL) {
  load(file=file.path(mvadir, "mlr", "mlrclassifiers.rData")) # mlrclassifiers
  classifiers <- sapply(strsplit(x = mlrclassifiers, split = ".", fixed = T), function(x) tail(x, n=1))
  logdebug("Removing bartMachine because I need to learn how to serialize it")
  logdebug("Removing extraTrees because it doesn't store fit correctly.")
  logdebug("Removing mda because it doesn't predict correctly")
  logdebug("Removing lda because it doesn't seem good for classification.")
  logdebug("Removing avNNet because it just doesn't seem to work well.")
  classifiers <- rmElement("bartMachine", classifiers)
  classifiers <- rmElement("extraTrees", classifiers)
  classifiers <- rmElement("mda", classifiers)
  classifiers <- rmElement("lda", classifiers)
  classifiers <- rmElement("avNNet", classifiers)
  badEffs <- c("dbnDNN", "saeDNN", "mlp", "nnTrain", "sparseLDA", "plsdaCaret")
  for ( classifier in badEffs ) {
    logdebug(paste("Removing",classifier,"because it always gives 0.5 ROC"))
    classifiers <- rmElement(classifier, classifiers)
  }
  
  if ( lvl == 1 ) { 
    if ( "ranger" %in% rownames(installed.packages()) ) {
      classifiers <- c("ctree", "glmnet", "naiveBayes", "ranger", "xgboost")
    } else {
      classifiers <- c("ctree", "glmnet", "naiveBayes", "randomForest", "xgboost")
    }
  }
  if ( lvl == 2 ) {
    if ( "ranger" %in% rownames(installed.packages()) ) {
      classifiers <- c("ctree", "glmnet", "naiveBayes", "ranger", "xgboost", "gbm", "rpart", "nnet", "cvglmnet")
    } else {
      classifiers <- c("ctree", "glmnet", "naiveBayes", "randomForest", "xgboost", "gbm", "rpart", "nnet", "cvglmnet")
    }
  }
  return( classifiers )
}


####################################################################################################################
#
# Create a Learner with Params
#
####################################################################################################################
createMLRLearner <- function(lrnname, traindata, problemType, positiveTarget, task, maxGrid, paramLVL) {
  logdebug(paste("  Creating MLR Learner for",lrnname,"using",problemType))
  
  mvadir <- file.path(getwd(), "mva")
  stopifnot(dir.exists(mvadir))
  
  files <- c("mlrParams.R")
  for ( ifile in files ) {
    fname <- file.path(mvadir, ifile)
    stopifnot(file.exists(fname))
    source(fname)
  }

  predict.type <- NULL
  if ( problemType == "linearRegression") {
    predict.type <- "response"
    par.vals  <- getMLRParamsForRegression(lrnname, traindata, task, len = maxGrid, lvl = paramLVL)
  }
  if ( problemType == "binaryClassification") {
    predict.type <- "prob"
    par.vals  <- getMLRParamsForClassification(lrnname, traindata, positiveTarget, task, len = maxGrid, lvl = paramLVL)
  }
  if ( problemType == "cluster") {
    predict.type <- "response"
    par.vals  <- getMLRParamsForCluster(lrnname, traindata, task, len = maxGrid, lvl = paramLVL)
  }
  if ( is.null(par.vals) ) {
    logdebug(paste("    Using default MLR hyper parameters for", lrnname,"with type:",predict.type))
    lrn     <- makeLearner(lrnname, predict.type = predict.type)
  } else {
    logdebug(paste("    Setting MLR hyper parameters for", lrnname,"with type:",predict.type))
    lrn <- setHyperPars(makeLearner(lrnname, predict.type = predict.type), par.vals = par.vals)
  }
  
  #lrn <- setMLRLearnerParams(lrn)
  return( lrn )
}



####################################################################################################################
#
# Trim Fit Information
#
####################################################################################################################
trimMLRFit <- function(fit, mva) {
  loginfo(paste("  Trimming MLR Fit for MVA:",mva))
  
  #print(showMem(fit))
  if ( mva == "glmnet" | mva == "lm" ) {
    fit[["learner.model"]][["call"]] <- NULL
  }
  #print(showMem(fit))
  return( fit )
}