mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "convData.R", "replaceNA.R", "dataProp.R", 
           "targetParams.R", "dataOps.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
#
# Get Formula
#
##############################################################################################################################
getFormulaText <- function(fdata, targetcol) {
  if ( is.null(targetcol) ) { return( NULL ) }
  logdebug(paste("    Getting formula text for column:",targetcol))
  pos <- loc(targetcol, colnames(fdata))
  if (is.null(pos) ) { stop(paste("Target",targetcol,"is not in the data.")) }
  text <- paste(colnames(fdata)[pos],"~", paste(colnames(fdata)[-pos], collapse= "+"))
  return( text )
}

getFormula <- function(fdata, target = NULL) {
  if ( is.null(target) ) { targetcol <- "TARGET" }
  else                   { targetcol <- target }

  logdebug(paste("    Getting formula for target column:",target))
  
  text <- getFormulaText(fdata = fdata, targetcol = targetcol)
  modelFormula <- as.formula(text)
  
  logdebug(paste("    Formula is:",text))

  return( modelFormula )
}

##############################################################################################################################
#
# Format Data
#
##############################################################################################################################
formatData <- function(fdata, targetcol, targetType, positiveTarget, fracTrain) {
  loginfo(paste("Formating data of size",getDimStr(fdata)))
  
  problemType <- getProblemType(fdata[,targetcol])
  
  
  loginfo(paste("  Target column",targetcol,"has",length(fdata[,targetcol]),"entries."))
  loginfo(paste("  Problem is of type:",problemType))
  if ( problemType == "binaryClassification" ) {
    loginfo(paste("  This is a binary classification problem."))
    loginfo(paste("  Number of positive target",positiveTarget,"entries is",sum(fdata[,targetcol]==positiveTarget)))
  } else if ( problemType == "cluster" ) {
    loginfo(paste("  This is a clustering problem."))
    loginfo(paste("  Summary of cluster data"))
    print(summary(fdata))
  } else if ( problemType == "linearRegression" ) {
    loginfo(paste("  This is a linear regression problem."))
    loginfo(paste("  Summary of regression target class",targetcol))
    print(summary(fdata[,targetcol]))
  } else {
    stop(paste("Problem type",problemType,"was not recognized!"))
  }
  

  pdim <- ncol(fdata)
  
  # 1) format target based on what we want
  pos        <- loc(targetcol, colnames(fdata))
  if ( is.null(pos) ) { targetdata <- NULL }
  else                { targetdata <- fdata[,pos] }
  if ( problemType == "binaryClassification") {
    loginfo(paste("  Formating target data to",targetType,"with target ->",positiveTarget))
    targetdata <- convColData(targetdata, to = targetType, positiveTarget = positiveTarget, positiveFactor = NULL)
  } else {
    loginfo(paste("  Not changing target data since this is",problemType)) 
  }
  
  
  # 2) format remaining data to numeric
  if ( problemType %in% c("linearRegression", "binaryClassification") ) {
    loginfo(paste("  Formating remaining data to numeric data."))
    if ( ncol(fdata) > 2 ) {
      remaindata <- fdata[,-pos]
      loginfo(paste("    There are",ncol(remaindata),"columns to format."))
    } else {
      colname <- rmElement(targetcol, colnames(fdata))
      remaindata <- data.frame(fdata[,-pos])
      colnames(remaindata) <- colname
      loginfo(paste("    There is only one column to format."))
    }
  } else { remaindata <- NULL }
  
  if ( !is.null(remaindata) ) {
    remaindata <- expandFactorData(remaindata, target = NULL, positiveTarget = NULL, ignores = NULL)
    for ( i in seq(ncol(remaindata)) ) {
      coldata <- remaindata[,i]
      coldata <- convColData(coldata, to = "binary", positiveTarget = NULL, positiveFactor = NULL)
      remaindata[,i] <- coldata
    }
    colclasses <- apply(remaindata, 2, class)
    if ( !any(c("numeric", "integer") %in% colclasses) ) {
      print(colclasses)
      stop("There are non numeric columns after calling convColData()")
    }
  }
  
  # 3) combine everything and renaming target column to TARGET
  loginfo(paste("  Combining data."))
  if ( !is.null(remaindata) ) { fdata <- cbind(remaindata, "TARGET"=targetdata) }

  # 4) replace NA
  loginfo(paste("  Replacing NAs."))
  fdata <- replaceNAs(cdata = fdata, targetcol = "TARGET")
  testReplaceNAs <- F
  if ( testReplaceNAs ) { print(summary(fdata)); stop("Testing Replace NAs") }
  if ( any(is.na(fdata)) ) { stop("There are NAs after calling ReplaceNAs()") }
  
  
  # 5) replace low variance
  loginfo(paste("  Replacing low variance"))
  fdata <- replaceLowVariance(cdata = fdata, fracTrain = fracTrain, targetcol = "TARGET", positiveTarget, problemType)
  testReplaceLowVar <- F
  if ( testReplaceLowVar ) {
    print(summary(fdata)); stop("Testing Replace Low Var")
  }
  
  
  ndim <- ncol(fdata)
  loginfo(paste("  Formatted data went from",pdim,"columns to",ndim,"columns"))

  return( fdata )
}



##############################################################################################################################
#
# Formating columns
#
##############################################################################################################################
getPositiveTarget <- function(targetType, truthdata) {
  logdebug(paste("  Getting positive target for target type:",targetType))
  
  positiveTarget <- NULL
  if ( targetType == "integer" ) { positiveTarget <- "1" }
  if ( targetType == "factor" )  { positiveTarget <- levels(truthdata)[1] }
  if ( targetType == "numeric" ) { positiveTarget <- NA }
  
  if ( is.null(positiveTarget) ) { 
    logerror(paste("Not sure what the positiveTarget is for",targetType))
  }
  
  logdebug(paste("Returning positiveTarget as",positiveTarget))
  return( positiveTarget )
}
  

formatTarget <- function(targetdata, target, targetType) {
  logdebug(paste("  Formatting target of type:",targetType))
  
  if ( targetType == "integer" ) {
    logdebug("    Formatting target to integer type.")
    targetdata <- setColToNumeric(targetdata, target)
  }
  if ( targetType == "factor" ) {
    logdebug("    Formatting target to factor type.")
    targetdata <- setColToFactor(targetdata, target)
  }
  if ( targetType == "numeric" ) {
    logdebug("    Formatting target to numeric type.")
    targetdata <- as.numeric(targetdata)
  }
  
  return( targetdata )
}

  
negateFactor <- function(value, positiveTarget) {
  logdebug("  Negating factor")
  
  retval <- NULL
  if ( value == positiveTarget ) { retval <- paste("NOT",positiveTarget,sep="") }
  if ( value == paste("NOT",positiveTarget,sep="") ) { retval <- positiveTarget }
  if ( is.null(retval) ) { stop(paste("Something bad happened in negateFactor:",value,positiveTarget)) }
  return( retval )
}


setBinaryTargetLevels <- function(target, rev = F) {
  levels <- c(target, paste("NOT",target,sep=""))
  if ( rev ) {
    levels <- c(paste("NOT",target,sep=""), target)
  }
  return( levels )
}










