mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}

################################################################################################################
#
# targetType
#
################################################################################################################
getTargetType <- function(targetdata, mva = NULL) {
  logdebug(paste("  Getting target type"))

  useCaret    <- get0("useCaret")
  useMLR      <- get0("useMLR")
  useNative   <- get0("useNative")
  
  problemType <- getProblemType(targetdata)
  
  targetType <- NULL
  if ( problemType == "linearRegression") {
    targetType <- "numeric"
  } else if ( problemType == "binaryClassification" ) {
    if ( useCaret ) {
      targetType <- "factor"
      if ( !is.null(mva) ) { if ( mva %in% c("xgboost") ) { targetType <- "integer" } }
    } else if ( useMLR ) {
      targetType <- "factor"
    } else { 
      targetType <- "factor"
    }
  } else if ( problemType == "cluster" ) {
    targetType <- "integer"
  } else {
    stop(paste("problem type",problemType,"is unknown."))
  }
  
  logdebug(paste("    Target type is",targetType))
  
  return( targetType )
}


################################################################################################################
#
# problemType
#
################################################################################################################
getProblemType <- function(tdata) {
  logdebug("  Getting problem type")
  if ( is.null(tdata) ) {
    problemType <- "cluster"
    return( problemType )
  }
  else {
    if ( class(tdata) == "data.frame") {
      if ( ncol(tdata) == 0 ) {
        problemType <- "cluster"
        return( problemType )
      } else {
        stop("No idea how to use this data to get problem type!")
      }
    } else {
      problemType <- "binaryClassification"
      if ( any(c("numeric","integer") %in% class(tdata)) ) {
        if ( length(unique(sample(x = tdata, size = min(50, length(tdata)), replace = T)) ) > 2 ) { problemType <- "linearRegression" }
      }
      logdebug(paste("    Returning problem type:",problemType))
      return( problemType )
    }
  }
}