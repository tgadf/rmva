mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("mlrBase.R", "caretBase.R", "performanceAnalysis.R", 
           "memory.R", "dataOps.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


################################################################################################################
#
# The Full Monty
#
################################################################################################################
getFullMVA <- function(mva, traindata, testdata, truthdata, targetcol, positiveTarget, targetType, ...) {
  options     <- unlist(list(...))
  useCaret    <- get0("useCaret")
  useMLR      <- get0("useMLR")
  useNative   <- get0("useNative")
  debug       <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  if ( !any(useCaret, useMLR, useNative) ) { stop("Need to use useCaret, useMLR, or useNative in trainMVA()") }
  
  if ( debug ) {
    if ( useCaret ) { loginfo(paste("Training, testing, and predicting with Caret",mva)) }
    if ( useMLR )   { loginfo(paste("Training, testing, and predicting with MLR",mva)) }
    if ( useCaret ) { loginfo(paste("Training, testing, and predicting with Native",mva)) }
    loginfo(paste("  Training data size:", paste(dim(traindata), collapse = " x")))
    loginfo(paste("  Testing data size: ", paste(dim(traindata), collapse = " x")))
    loginfo(paste("  Positive Target:   ", positiveTarget))
    loginfo(paste("  Target Type:       ", targetType))
    loginfo("\n")
  }

  
  ##########################################################################################
  # Train MVA section
  ##########################################################################################
  retval  <- trainMVA(mva = mva, traindata = traindata, targetcol = targetcol, 
                      positiveTarget = positiveTarget, c(options, targetType))
  fits    <- getFits(retval)

  
  ##########################################################################################
  # Test MVA section
  ##########################################################################################
  retval      <- testMVA(fit = fits, truthdata = truthdata, testdata = testdata, targetcol = targetcol, 
                         positiveTarget = positiveTarget, targetType = targetType, options)
  testResults <- getTests(retval)
  
  
  ##########################################################################################
  # Performance MVA section
  ##########################################################################################
  retval      <- performanceMVA(testResults = testResults, truthdata = truthdata, 
                                positiveTarget = positiveTarget, options)
  performance <- getPerfs(retval)
  
  return( performance )
}


################################################################################################################
#
# Allowed Classifiers
#
################################################################################################################
getAllowedClassifiers <- function(mva) {
  if ( mva %in% c("avnnet", "ctree", "gbm") ) { allowed <- c("Caret") }
  else { allowed <- c("Native", "Caret") }
  allowed <- c("MLR", allowed)
  return( allowed )
}




################################################################################################################
#
# trainMVA
#
################################################################################################################
getTimes  <- function(items = NULL) {
  if ( is.null(items) ) {
    writeLines("Using default value of retvals as input to getFits()")
    items <- get0("retvals")
  }
  if ( length(items) == 0 ) { stop("No fits to return!") }
  times <- lapply(items, function(x) x[["time"]])
  loginfo(paste("Returning",length(times),"MVA times."))
  return(times)
}

getFits  <- function(items = NULL) {
  if ( is.null(items) ) {
    writeLines("Using default value of retvals as input to getFits()")
    items <- get0("retvals")
  }
  if ( length(items) == 0 ) { stop("No fits to return!") }
  fits <- lapply(items, function(x) x[["fit"]])
  loginfo(paste("Returning",length(fits),"MVA fits."))
  return(fits)
}

combineFits <- function(fits, newfits) {
  logdebug(paste("Combining fits of lengths:",length(fits),"and",length(newfits)))
  for ( name in names(newfits) ) {
    fits[[name]] <- newfits[[name]]
  }
  logdebug(paste("Combined fits has length:",length(fits)))
  return( fits )
}

trainMVA <- function(mvas, traindata, targetcol, positiveTarget, maxGrid) {
  file.info(paste("Training",length(mvas),"MVAs with data of size:",getDimStr(traindata)))
  useCaret    <- get0("useCaret")
  useMLR      <- get0("useMLR")
  useNative   <- get0("useNative")
  paramLVL    <- get0("paramLVL")
  tdata       <- traindata[,targetcol]
  problemType <- getProblemType(tdata)
  
  if ( !any(useCaret, useMLR, useNative) ) { stop("Need to use useCaret, useMLR, or useNative in trainMVA()") }
  
  require(e1071)

  if ( is.null(targetcol) ) { 
    formulaText  <- NULL
    modelFormula <- NULL
  }
  else {
    formulaText  <- getFormulaText(fdata = traindata, targetcol = targetcol)
    modelFormula <- getFormula(fdata = traindata, target = targetcol)
  }
  
  retval <- list()
  for ( mva in mvas ) {
    gc()
    loginfo(paste("  Training",mva,"for",problemType))
    if ( useCaret )  { logdebug("    Using Caret Classifiers") }
    if ( useMLR )    { logdebug("    Using MLR Classifiers") }
    if ( useNative ) { logdebug("    Using Native Classifiers") }
    if ( !is.null(formulaText) ) { logdebug(paste("    Using formula:",formulaText)) }
    logdebug(paste("    Using a tunning grid with",maxGrid,"rows."))
    memUsed()
    
    ##############################################################################
    ## MLR
    ##############################################################################
    if ( useMLR ) {
      ftime <- system.time({
        mlrval <- trainMLR(mva, modelFormula, traindata, targetcol, positiveTarget, maxGrid, paramLVL)
        fit    <- mlrval
      })
    }
    
    
    ##############################################################################
    ## Caret
    ##############################################################################
    if ( useCaret ) {
      ftime <- system.time({
        carval <- trainCaret(mva, modelFormula, traindata, targetcol, positiveTarget, maxGrid)
        fit    <- carval
      })
    }
    

    ##############################################################################
    ## Native
    ##############################################################################
    if ( useNative ) {
      fname <- file.path(mvadir, paste(mva, "R", sep="."))
      stopifnot(file.exists(fname))
      source(fname)
      fname <- paste("train",getMVAname(mva),sep="")
      trainFunc <- get(fname)
      if ( is.null(trainFunc) ) { stop(paste("Could not find train function for",mva)) }
      ftime <- system.time({  fit <- trainFunc(formula, traindata, target, problemType, options)  })
      imp <- NULL
    }
    
    loginfo(paste("  Training",mva,"for",problemType,"done."))
    retval[[mva]] <- list("fit"=fit, "time"=ftime)
    gc()
    memUsed()
  }

  loginfo(paste("Trained",length(retval),"classifiers."))
  return( retval )
}



################################################################################################################
#
# testMVA
#
################################################################################################################
getTests  <- function(items = NULL) {
  if ( is.null(items) ) {
    loginfo("Using default value of retvals as input to getTests()")
    items <- get0("retvals")
  }
  if ( length(items) == 0 ) { warning("No test results to return!") }
  testResults <- lapply(items, function(x) x[["testResults"]])
  loginfo(paste("Returning",length(testResults),"MVA test results."))
  return(testResults)
}

testMVA  <- function(fits, truthdata, testdata, targetcol, positiveTarget, targetType, njobs) {
  file.info(paste("Testing",length(fits),"MVAs with data of size:",getDimStr(traindata)))
  useCaret    <- get0("useCaret")
  useMLR      <- get0("useMLR")
  useNative   <- get0("useNative")
  problemType    <- getProblemType(truthdata) 
  if ( !any(useCaret, useMLR, useNative) ) { stop("Need to use useCaret, useMLR, or useNative in trainMVA()") }

  mvadir <- file.path(getwd(), "mva")
  stopifnot(dir.exists(mvadir))
  
  retval <- list()
  for ( mva in names(fits) ) {
    loginfo(paste("Testing",mva,"for",problemType))
    fit <- fits[[mva]]
    
    if ( useMLR ) {
      ftime <- system.time({  testResults <- testMLR(mva, fit, testdata, problemType, targetcol, positiveTarget, njobs)  })
    } else if ( useCaret ) {
      ftime <- system.time({  testResults <- testCaret(mva, fit, testdata, problemType, targetcol, positiveTarget, njobs)  })
    } else {
      fname <- file.path(mvadir, paste(mva, "R", sep="."))
      stopifnot(file.exists(fname))
      source(fname)
      fname <- paste("get",getMVAname(mva),"TestResults", sep="")
      testFunc <- get(fname)
      if ( is.null(testFunc) ) { stop(paste("Could not find test function for",mva)) }
      ftime <- system.time({  testResults <- testFunc(fit, testdata, problemType, target, positiveTarget)  })
    }
    
    retval[[mva]] <- list("time"=ftime, "testResults"=testResults)
    loginfo(paste("Testing",mva,"for",problemType,"Done."))
  }
  
  loginfo(paste("  Tested",length(retval),"MVAs"))
  
  return(retval)
  
}



################################################################################################################
#
# performanceMVA
#
################################################################################################################
getPerfs  <- function(items = NULL) {
  if ( is.null(items) ) {
    writeLines("Using default value of retvals as input to getPerfs()")
    items <- get0("retvals")
  }
  if ( length(items) == 0 ) { stop("No fits to return!") }
  perfs <- lapply(items, function(x) x[["performance"]])
  loginfo(paste("Returning",length(perfs),"MVA performance results."))
  return(perfs)
}

performanceMVA <- function(testResults, truthdata, positiveTarget) {
  loginfo("Determine MVA performances.")
  classifiers <- names(testResults)
  retval <- list()
  for ( mva in classifiers ) {
    loginfo(paste("  Determine MVA performance for",mva))
    performance <- getMVAperformance(testResults = testResults[[mva]], truthLabels = truthdata, positiveTarget = positiveTarget, mva = mva)
    retval[[mva]] <- list("performance"=performance)
  }
  
  loginfo(paste("Determine MVA performances for",length(retval),"classifiers."))
  return( retval )
}