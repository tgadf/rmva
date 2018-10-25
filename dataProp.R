mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
#
# Data properties
#
##############################################################################################################################
replaceLowVariance <- function(cdata, fracTrain, targetcol, positiveTarget, problemType) {
  file.info("  Replacing low variance columns.")
  if ( problemType == "cluster" ) { return( cdata ) }

  targetdata <- cdata[,targetcol]
  if ( problemType == "binaryClassification" ) {
    minInDownSampledData <- 5
    nPositive  <- sum(targetdata == positiveTarget)
    nNegative  <- sum(targetdata != positiveTarget)
    downFrac <- max(nNegative/nPositive , nPositive/nNegative)
    logdebug(paste("    Min In Down Sample",minInDownSampledData))
    minInDownSampledData <- minInDownSampledData * downFrac
  }
  minInDownSampledData <- 15
  logdebug(paste("    Min In Down Sample",minInDownSampledData))
  fracTest = 1 - fracTrain
  splitFrac <- min(fracTrain , fracTest)
  minInDownSampledData <- minInDownSampledData / splitFrac
  logdebug(paste("    Min In Down Sample",minInDownSampledData))
  
  minLevel <- as.integer(minInDownSampledData)
  islowVars <- isLowVarData(cdata, minLevel= minLevel)
  for ( colname in colnames(cdata)[islowVars] ) { logdebug(paste("    Removing",colname,"due to low variance (",minLevel,").")) }

  file.info(paste("    Removing",sum(islowVars),"columns due to low varince."))
  file.info(paste("    Before removal there are",ncol(cdata),"columns."))
  fdata    <- cdata[,!islowVars]
  file.info(paste("    After removal there are now",ncol(fdata),"columns."))
  return( fdata )
}


isLowVarData <- function(cdata, minLevel) {
  logdebug("    Determine data properties.")
  
  lowvars <- c()
  for ( i in seq(ncol(cdata)) ) {
    coldata  <- cdata[,i]
    colname  <- colnames(cdata)[i]
    if ( colname == "TARGET" ) { islowVar <- F }
    else {
      islowVar <- isLowVarColData(coldata, colname = colname, minLevel = minLevel)
    }
    lowvars <- c(lowvars, islowVar)
  }
  
  return( lowvars )
}
  
isLowVarColData <- function(coldata, colname = NULL, minLevel) {
  if ( is.null(colname) ) { logdebug("    Determine column data variance.") }
  else                    { logdebug(paste("    Determine",colname,"variance.")) }

  colvar <- var(as.numeric(coldata), na.rm = T)
  lowvar <- F
  cdata  <- class(coldata)
  
  nUnique <- length(unique(sample(coldata, size = 500, replace = T)))
  if ( nUnique == 1 ) {
    lowvar <- T
    if ( lowvar ) { logdebug(paste("    Low",colname,"variance because only one unique value.")) }
  }
  else if ( nUnique == 2 ) {
    minVal <- min(table(coldata))
    if ( minVal < minLevel ) { lowvar <- T }
    if ( lowvar ) { logdebug(paste("    Low",colname,"variance ->",minVal,"<",minLevel)) }
  }
  return( lowvar )
}