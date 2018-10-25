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
# MLR Params
#
####################################################################################################################
getMLRParamsForRegression <- function(lrnname, traindata, task, len, lvl) {
  logdebug(paste("  Getting MLR",lrnname,"hyperparameters."))
  
  rdesc  <- makeResampleDesc("Bootstrap", iters = 1)
  num_ps <- getMLRParams(classifier = lrnname, traindata, lvl)
  if ( is.null(num_ps) ) { return( NULL ) }
  
  ctrl = makeTuneControlRandom(maxit = len)
  res = tuneParams(lrnname, task = task, resampling = rdesc, par.set = num_ps,
                   control = ctrl, show.info = T)
  par.val <- res[["x"]]
  return( par.val )
}

getMLRParamsForCluster <- function(lrnname, traindata, task, len, lvl) {
  logdebug(paste("  Getting MLR",lrnname,"hyperparameters."))
  
  rdesc  <- makeResampleDesc("Bootstrap", iters = 1)
  num_ps <- getMLRParams(classifier = lrnname, traindata, lvl)
  if ( is.null(num_ps) ) { return( NULL ) }
  
  ctrl = makeTuneControlRandom(maxit = len)
  res = tuneParams(lrnname, task = task, resampling = rdesc, par.set = num_ps,
                   control = ctrl, show.info = T)
  par.val <- res[["x"]]
  return( par.val )
}

getMLRParamsForClassification <- function(lrnname, traindata, positiveTarget, task, len, lvl) {
  logdebug(paste("  Getting MLR",lrnname,"hyperparameters."))
  
  rdesc  <- makeResampleDesc("Bootstrap", iters = 1)
  num_ps <- getMLRParams(classifier = lrnname, traindata, lvl)
  if ( is.null(num_ps) ) { return( NULL ) }
  
  ctrl = makeTuneControlRandom(maxit = len)
  res = tuneParams(lrnname, task = task, resampling = rdesc, par.set = num_ps,
                   control = ctrl, measures = list(acc, setAggregation(acc, test.sd)), show.info = T)
  par.val <- res[["x"]]
  return( par.val )
}


####################################################################################################################
#
# MLR Params
#
####################################################################################################################
setMLRLearnerParams <- function(lrn, lrnname) {
  loginfo(paste("  Checking for set hyper params for",lrnname))
  mvaname <- unlist(strsplit(lrnname, split = ".", fixed = T))[2]
  if ( mvaname == "bcart") {
    loginfo(paste("    Turning off prediction for",lrnname))
    lrn = setHyperPars(lrn, pred.n = F)
  }
  return( lrn )
}



####################################################################################################################
#
# MLR Params (Specific to MVA)
#
####################################################################################################################
getMtry    <- function(nvars) {
  if ( nvars < 7 ) { return( seq(1, nvars) ) }
  else             { return( unique(floor(seq(min(nvars,2), to = nvars, length = 7))) ) }
}
getMtryMin <- function(nvars) {
  mtrys <- getMtry(nvars)
  if ( length(mtrys) > 2 ) { return( min(mtrys[2], 2) ) }
  else                     { return( min(min(mtrys), 2) ) }
}
getMtryMax <- function(nvars) { 
  mtrys <- getMtry(nvars)
  if ( length(mtrys) > 5 ) { return( min(mtrys[5], 20) ) }
  else                     { return( min(max(mtrys), 20) ) }
}

getMLRParams <- function(classifier, traindata, lvl=3) {
  num_ps    <- NULL
  nvars     <- ncol(traindata)
  lowerVars <- getMtryMin(nvars)
  upperVars <- min(c(getMtryMax(nvars), 30))
  maxDepth  <- 5
  maxTrees  <- 5
  maxRounds <- 3
  minObs    <- 2
  if ( lvl == 1 ) {
    upperVars <- min(5, upperVars)
    maxDepth  <- 2
    maxTrees  <- 1
    maxRounds <- 1
  } else if ( lvl == 2 ) {
    upperVars <- min(10, upperVars)
    maxDepth  <- 3
    maxTrees  <- 2
    maxRounds <- 2
  }
  
  mvaname <- unlist(strsplit(classifier, split = ".", fixed = T))[2]
  
  depthParam <- 
    
    if ( mvaname == "ada" ) {
      num_ps <- makeParamSet(makeIntegerParam("iter", lower = 1, upper = maxRounds, trafo = function(x) 50*x))
      num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("loss", values = c("exponential", "logistic"))))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("maxdepth", lower = 1, upper = maxDepth, trafo = function(x) 2*x)))
    } else if ( mvaname == "bartMachine" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeIntegerParam("num_trees", lower = 1, upper = 3, trafo = function(x) 50*as.integer(x)))
      #num_ps <- c(num_ps, makeParamSet(makeIntegerParam("k", lower = 0, upper = 3)))
      #num_ps <- c(num_ps, makeParamSet(makeNumericParam("q", lower = 0.75, upper = 1)))
      #num_ps <- c(num_ps, makeParamSet(makeIntegerParam("nu", lower = 1, upper = 5)))
    } else if ( mvaname == "bcart" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("bprior", values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")))
    } else if ( mvaname == "bgp" ) { # blm, btgp, btgpllm, btlm
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("bprior", values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("meanfn", values = c("constant", "linear"))))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("corr", values = c("exp", "expsep", "matern", "sim"))))
    } else if ( mvaname == "blm" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("bprior", values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("meanfn", values = c("constant", "linear"))))
    } else if ( mvaname == "btgp" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("bprior", values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("meanfn", values = c("constant", "linear"))))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("corr", values = c("exp", "expsep", "matern", "sim"))))
    } else if ( mvaname == "btgpllm" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("bprior", values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("meanfn", values = c("constant", "linear"))))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("corr", values = c("exp", "expsep", "matern", "sim"))))
    } else if ( mvaname == "btlm" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("bprior", values = c("b0", "b0not", "bflat", "bmle", "bmznot", "bmzt")))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("meanfn", values = c("constant", "linear"))))
    } else if ( mvaname == "blackboost" ) {
      #num_ps <- makeParamSet(makeDiscreteLearnerParam("family", default = "Gaussian", values = c("Gaussian", "Laplace", "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle")))
      num_ps <- makeParamSet(makeIntegerParam("mstop", lower = 1, upper = maxRounds, trafo = function(x) 50*x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("maxdepth", lower = 1, upper = maxDepth, trafo = function(x) 2*x)))
    } else if ( mvaname == "boosting" ) {
      num_ps <- makeParamSet(makeDiscreteParam("coeflearn", values = c("Breiman", "Freund", "Zhu"), default = "Breiman"))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("maxdepth", lower = 1, upper = maxDepth, trafo = function(x) 2*x)))
    } else if ( mvaname == "brnn" ) {
      num_ps <- makeParamSet(makeIntegerParam("neurons", lower = 1, upper = 3))
    } else if ( mvaname == "bst" ) {
      num_ps <- makeParamSet(makeNumericParam("cost", lower = 0, upper = 1))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("mstop", lower = 1, upper = maxRounds, trafo = function(x) 50*x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("maxdepth", lower = 1, upper = maxDepth, trafo = function(x) 2*x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("nu", lower = -3, upper = -1, trafo = function(x) 10^x)))
    } else if ( mvaname == "cforest" ) {
      num_ps <- makeParamSet(makeIntegerParam("mtry", lower = lowerVars, upper = upperVars, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("teststat", values = c("quad", "max"), default = "quad")))
      num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("testtype", values = c("Bonferroni", "Univariate", "Teststatistic"), default = "Univariate")))
    } else if ( mvaname == "ctree" ) {
      num_ps <- makeParamSet(makeNumericParam("mincriterion", lower = 0.9, upper = 1.0, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("teststat", values = c("quad", "max"))))
      num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("testtype", values = c("Bonferroni", "Univariate", "Teststatistic"))))
    } else if ( mvaname == "cubist" ) {
      num_ps <- makeParamSet(makeIntegerParam("committees", lower = 0, upper = 2, trafo = function(x) 10^x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("rules", lower = 0, upper = 2, trafo = function(x) 10^x)))
    } else if ( mvaname == "earth" ) {
      num_ps <- makeParamSet(makeDiscreteParam("pmethod", default="backward", values = c("backward", "none", "exhaustive", "forward", "seqrep")))
    } else if ( mvaname == "elmNN" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("actfun", default = "sig", values = c("sig", "sin", "radbas", "hardlim", "hardlims", "satlins", "tansig", "tribas", "poslin", "purelin")))
    } else if ( mvaname == "extraTrees" ) {
      num_ps <- makeParamSet(makeIntegerParam("mtry", lower = lowerVars, upper = upperVars, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("ntree", lower = 1, upper = maxTrees, trafo = function(x) 100*x)))
    } else if ( mvaname == "evtree" ) {
      num_ps <- makeParamSet(makeIntegerParam("alpha", lower = 0, upper = 3, trafo = function(x) x))
    } else if ( mvaname == "gamboost" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("family", values = c("Gaussian", "Laplace", "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle")))
      #num_ps <- makeParamSet(makeIntegerParam("mstop", lower = 1, upper = 3, trafo = function(x) 50*as.integer(x)))
      #num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("baselearner", values = c("bbs", "bols", "btree"))))
    } else if ( mvaname == "gausspr" ) {
      num_ps <- makeParamSet(makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")))
    } else if ( mvaname == "gbm" ) {
      num_ps <- makeParamSet(makeIntegerParam("n.trees", lower = 1, upper = 5, trafo = function(x) 100*x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("interaction.depth", lower = 1, upper = maxDepth, trafo = function(x) 2*x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("n.minobsinnode", lower = 1, upper = minObs, trafo = function(x) x)))
    } else if ( mvaname == "glm" ) {
      ## No non-positive values not allowed for the 'gamma' family
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("family", values = c("gaussian", "Gamma", "poisson")))
    } else if ( mvaname == "glmboost" ) {
      #num_ps <- makeParamSet(makeDiscreteParam("family", values = c("Gaussian", "Laplace", "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle")))
      num_ps <- makeParamSet(makeIntegerParam("mstop", lower = 1, upper = maxRounds, trafo = function(x) 50*x))
    } else if ( mvaname == "glmnet" ) {
      num_ps <- makeParamSet(makeNumericParam("alpha", lower = 0.0, upper = 1.0, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("lambda", lower = 0, upper = 3, trafo = function(x) x)))
      #if ( classifier == "regr.glmnet" ) { num_ps <- c(num_ps, makeParamSet(makeDiscreteParam("family", values = c("gaussian", "poisson")))) }
    } else if ( mvaname == "GPfit" ) {
      num_ps <- makeParamSet(makeDiscreteLearnerParam("type", values = c("exponential", "matern")))
    } else if ( mvaname == "kknn" ) {
      num_ps <- makeParamSet(makeNumericParam("k", lower = lowerVars, upper = upperVars, trafo = function(x) as.integer(x)))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("distance", lower = 0, upper = sqrt(nvars), trafo = function(x) x)))
    } else if ( mvaname == "km" ) {
      num_ps <- NULL # DiceKriging
    } else if ( mvaname == "knn" ) {
      num_ps <- makeParamSet(makeNumericParam("k", lower = 1, upper = upperVars, trafo = function(x) as.integer(x)))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("l", lower = 0, upper = sqrt(nvars), trafo = function(x) x)))
    } else if ( mvaname == "ksvm" ) {
      if ( classifier == "classif.ksvm" ) {
        num_ps <- makeParamSet(makeDiscreteParam("type", default = "C-svc", values = c("C-svc", "nu-svc", "C-bsvc")))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("C", lower = -2, upper = 2, trafo = function(x) 10^x)))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("nu", lower = -2, upper = 0, trafo = function(x) 10^x)))
      }
      if ( classifier == "regr.ksvm" ) {
        num_ps <- makeParamSet(makeDiscreteParam("type", default = "eps-svr", values = c("eps-svr", "nu-svr", "eps-bsvr")))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("C", lower = -2, upper = 2, trafo = function(x) 10^x)))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("nu", lower = -2, upper = 0, trafo = function(x) 10^x)))
      }
    } else if ( mvaname == "laGP" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeNumericParam("cost", lower = -1, upper = 1, trafo = function(x) 10^x))
      #num_ps <- c(num_ps, makeParamSet(makeNumericParam("epsilon", lower = -3, upper = -1, trafo = function(x) 10^x)))
    } else if ( mvaname == "LiblineaRL2L1SVR" ) {
      num_ps <- makeParamSet(makeNumericParam("cost", lower = -1, upper = 1, trafo = function(x) 10^x))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("epsilon", lower = -3, upper = -1, trafo = function(x) 10^x)))
    } else if ( mvaname == "LiblineaRL2L2SVR" ) {
      num_ps <- makeParamSet(makeNumericParam("cost", lower = -1, upper = 1, trafo = function(x) 10^x))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("epsilon", lower = -3, upper = -1, trafo = function(x) 10^x)))
    } else if ( mvaname == "mars" ) {
      num_ps <- makeParamSet(makeIntegerParam("degree", lower = 1, upper = 3, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("nk", lower = 1, upper = 3, trafo = function(x) 2^x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("penalty", lower = -2, upper = 2, trafo = function(x) 2^x)))
    } else if ( mvaname == "mob" ) {
      num_ps <- makeParamSet(makeNumericParam("alpha", lower = -3, upper = -1, trafo = function(x) 5*10^as.integer(x)))
    } else if ( mvaname == "nnet" ) {
      num_ps <- makeParamSet(makeIntegerParam("size", lower = getMtryMin(nvars), upper = getMtryMax(nvars), trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("decay", lower = -2, upper = 0, trafo = function(x) 10^x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("MaxNWts", lower = 100*nvars, upper = 100*nvars)))
    } else if ( mvaname == "pcr" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeIntegerParam("ncomp", lower = 1, upper = getMtryMax(nvars)))
    } else if ( mvaname == "plsr" ) {
      num_ps <- NULL
    } else if ( mvaname == "plr" ) {
      num_ps <- makeParamSet(makeDiscreteParam("cp.type", values = c("bic", "aic"), default = "bic"))
    } else if ( mvaname == "randomForest" ) {
      num_ps <- makeParamSet(makeIntegerParam("mtry", lower = lowerVars, upper = upperVars, trafo = function(x) x))
    } else if ( mvaname == "ranger" ) {
      num_ps <- makeParamSet(makeIntegerParam("mtry", lower = lowerVars, upper = upperVars, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("num.trees", lower = 1, upper = maxTrees, trafo = function(x) 100*x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("min.node.size", lower = 1, upper = minObs)))
    } else if ( mvaname == "rda" ) {
      num_ps <- makeParamSet(makeNumericParam("lambda", lower = 0, upper = 1))
      num_ps <- c(num_ps, makeParamSet(makeNumericParam("gamma", lower = 0, upper = 1)))
    } else if ( mvaname == "rknn" ) {
      num_ps <- makeParamSet(makeIntegerParam("mtry", lower = lowerVars, upper = upperVars, trafo = function(x) as.integer(x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("k", lower = 1, upper = 10)))
    } else if ( mvaname == "rotationForest" ) {
      num_ps <- makeParamSet(makeIntegerParam("K", lower = lowerVars, upper = upperVars, trafo = function(x) x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("L", lower = 1, upper = nvars, trafo = function(x) x)))
    } else if ( mvaname == "rpart" ) {
      num_ps <- makeParamSet(makeIntegerParam("maxdepth", lower = 1, upper = maxDepth, trafo = function(x) 2*x))
    } else if ( mvaname == "rsm" ) {
      #num_ps <- NULL
      num_ps <- makeParamSet(makeDiscreteParam("modelfun", values = c("FO", "TWI", "SO")))
    } else if ( mvaname == "rvm" ) {
      num_ps <- NULL
      #num_ps <- makeParamSet(makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")))
      #num_ps <- c(num_ps, makeParamSet(makeIntegerParam("degree", lower = 1, upper = 5)))
    } else if ( mvaname == "sda" ) {
      num_ps <- makeParamSet(makeNumericParam("lambda", lower = 0, upper = 1))
    } else if ( mvaname == "svm" ) {
      if ( classifier == "classif.svm" ) {
        num_ps <- makeParamSet(makeDiscreteParam("type", default = "C-classification", values = c("C-classification", "nu-classification")))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("cost", lower = -2, upper = 2, trafo = function(x) 10^x)))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("nu", lower = -2, upper = 0, trafo = function(x) 10^x)))
      } 
      if ( classifier == "regr.svm" ) {
        num_ps <- makeParamSet(makeDiscreteParam("type", default = "eps-regression", values = c("eps-regression", "nu-regression")))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("epsilon", lower = -2, upper = 0, trafo = function(x) 10^x)))
        num_ps <- c(num_ps, makeParamSet(makeNumericParam("nu", lower = -2, upper = 0, trafo = function(x) 10^x)))
      } 
    } else if ( mvaname == "slim" ) {
      num_ps <- makeParamSet(makeDiscreteParam("method", values = c("lq", "dantzig", "lasso")))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("nlambda", lower = lowerVars, upper = upperVars, trafo = function(x) as.integer(x))))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("rho", lower = lowerVars, upper = upperVars, trafo = function(x) as.integer(x))))
    }  else if ( mvaname == "xgboost" ) {
      #num_ps <- makeParamSet(makeDiscreteParam("booster", default = "gbtree", values = c("gbtree", "gblinear")))
      num_ps <- makeParamSet(makeNumericParam("eta", lower = -2, upper = -1, trafo = function(x) 3*10^x))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("max_depth", lower = 1, upper = maxDepth, trafo = function(x) 2*x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("min_child_weight",lower = 1, upper = minObs, trafo = function(x) x)))
      num_ps <- c(num_ps, makeParamSet(makeIntegerParam("nrounds",lower=1, upper=maxRounds, trafo = function(x) 50*x)))
    }
  
  return( num_ps )
}

