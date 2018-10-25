mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "dataOps.R", "logger.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
#
# Replace NA
#
##############################################################################################################################
replaceNAs <- function(cdata, targetcol = NULL) {
  loginfo(paste("  Replacing NAs in dataset of size",getDimStr(cdata)))

  targetdata <- NULL  
  if ( !is.null(targetcol) ) {
    if ( targetcol %in% colnames(cdata) ) {
      targetdata <- cdata[,targetcol]
    }
  }
  for ( i in seq_along(colnames(cdata)) ) {
    coldata   <- cdata[,i]
    colname   <- colnames(cdata)[i]
    newdata   <- replaceColNA(coldata = coldata, colname = colname, targetdata = targetdata)
    if ( any(is.na(newdata)) ) { logerror(paste("There are NAs in column",colnames(cdata)[i])) }
    cdata[,i] <- newdata
  }
  return( cdata )
}

replaceColNA <- function(coldata, colname, targetdata = NULL, useAvg = F, useGAvg = T, useZero = F) {
  logdebug("    Replace Column NA values")
  
  if ( any(is.na(coldata)) == F ) { return( coldata ) }
  Nna <- sum(is.na(coldata))
  loginfo(paste("    Found",Nna,"NAs in column",colname,"data."))
  
  if ( !any(useAvg,useZero,useGAvg) ) {
    if ( is.null(targetdata) ) { useGAvg <- T }
    else {
      if ( class(targetdata) == "numeric" ) { useGAvg <- T }
      else                                  { useAvg  <- T }
    }
  }
  
  if ( useGAvg ) { loginfo("      Replacing NAs with global average.") }
  if ( useAvg )  { loginfo("      Replacing NAs with categorical averages.") }
  if ( useZero ) { loginfo("      Replacing NAs with 0.") }
  
  if ( useZero ) { 
    is.na(coldata) <- 0
    return( coldata )
  }
  
  if ( useGAvg ) {
    value <- signif(mean(coldata, na.rm=T), 4)
    logdebug(paste("    Replacing all NAs in column",colname,"with",value))
    coldata[is.na(coldata)] <- value
    return( coldata )
  }
  
  if ( useAvg ) {
    avgs <- tapply(coldata, targetdata, function(x) round(mean(x, na.rm=T), digits = 4))
    for ( i in seq_along(avgs) ) {
      category <- names(avgs)[i]
      value    <- signif(avgs[i], 4)
      logdebug(paste("    Replacing all NAs in column",colname,"with",value))
      coldata[targetdata == category & is.na(coldata)] <- value
    }
    return( coldata )
  }
  
  logdebug("    Didn't alter the column in replaceColNA()")
  return( coldata )
}