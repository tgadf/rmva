mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
# convert data
##############################################################################################################################
convColData <- function(coldata, to, positiveTarget = NULL, positiveFactor = NULL) {
  logdebug(paste("  Converting column to type:",to))
  
  cdata <- class(coldata)
  if ( to == "integer" | to == "int" | to == "numeric" | to == "num" ) { to <- "binary" }
  
  if ( to == "binary" | to == "bin" ) {
    if ( "logical" %in% cdata ) { 
      coldata <- convLogicaltoBinary(coldata, positiveTarget = positiveTarget)
    }
    if ( "factor" %in% cdata ) { 
      coldata <- convFactortoBinary(coldata, positiveTarget = positiveTarget, positiveFactor = positiveFactor)
    }
  }
  
  if ( to == "factor" ) {
    if ( "ordered" %in% cdata ) { coldata <- convOrderedToFactor(coldata = coldata) }
    if ( any(c("integer","numeric") %in% cdata) ) {
      coldata <- convNumerictoBinaryFactor(coldata, positiveTarget = positiveTarget, positiveFactor = positiveFactor)
    }
    if ( "logical" %in% cdata ) {
      coldata <- convLogicaltoBinaryFactor(coldata, positiveTarget = positiveTarget, positiveFactor = positiveFactor)
    }
    if ( "factor" %in% cdata ) {
      coldata <- convFactortoBinaryFactor(coldata, positiveTarget = positiveTarget, positiveFactor = positiveFactor)
    }
    if ( "character" %in% cdata ) {
      coldata <- as.factor(coldata)
      coldata <- convFactortoBinaryFactor(coldata, positiveTarget = positiveTarget, positiveFactor = positiveFactor)
    }
  }
  
  return( coldata )
}


##############################################################################################################################
# get highest factor
##############################################################################################################################
getMostLikelyFactor <- function(coldata, N=1000) {
  logdebug(paste("  Getting most likely factor using",N,"/",length(coldata)))

  hdata <- head(coldata, n=min(N, length(coldata)))
  freq  <- table(hdata)
  if ( length(freq) < 20 ) {
    val <- head(sort(freq, decreasing = T), n = 1)
    logdebug(paste("  Returning most likely factor (< 20):",names(val)))
    return( names(val) )
  }
  logdebug(paste("  Returning first factor (>=20):",coldata[1]))
  return( coldata[1] )
}


##############################################################################################################################
# convert ordered data to factor
##############################################################################################################################
convOrderedToFactor <- function(coldata) {
  logdebug("  Converting ordered factor column to factor type")
  
  cdata <- class(coldata)
  if ( "ordered" %in% cdata ) {
    logdebug("    Converting ordered data to factor.")
    coldata <- factor(coldata, ordered = F)
  }
  return( coldata )
}


##############################################################################################################################
# convert data to binary
##############################################################################################################################
convLogicaltoBinary <- function(coldata, positiveTarget = T) {
  logdebug("  Converting logical column to binary integer type")
  
  cdata   <- class(coldata)
  if ( "logical" %in% cdata ) {
    logdebug(paste("    Converting numeric data to binary with pos =",positiveTarget))
    coldata <- ifelse(coldata == positiveTarget, 1, 0)
  }
  return( coldata )
}

convNumerictoBinary <- function(coldata, positiveTarget = NULL) {
  logdebug("  Converting numeric column to binary integer type")

  cdata   <- class(coldata)
  if ( any(c("numeric","integer") %in% cdata) ) {
    if ( is.null(positiveTarget) ) { positiveTarget <- getMostLikelyFactor(coldata, options) }
    logdebug(paste("    Converting numeric data to binary with pos =",positiveTarget))
    coldata <- ifelse(coldata == positiveTarget, 1, 0)
  }
  return( coldata )
}

convFactortoBinary <- function(coldata, positiveTarget = NULL) {
  logdebug("  Converting factor column to binary integer type")
  
  cdata   <- class(coldata)
  if ( "factor" %in% cdata ) {
    if ( is.null(positiveTarget) ) { positiveTarget <- getMostLikelyFactor(coldata, options) }
    logdebug(paste("    Converting factor data to binary with pos =",positiveTarget))
    coldata <- ifelse(coldata == positiveTarget, 1, 0)
  }
  return( coldata )
}



##############################################################################################################################
# convert data to binary factor
##############################################################################################################################
convFactortoBinaryFactor <- function(coldata, positiveTarget = NULL, positiveFactor = NULL) {
  logdebug("  Converting factor column to binary factor type")
  
  cdata   <- class(coldata)
  if ( "factor" %in% cdata ) {
    if ( is.null(positiveTarget) ) { positiveTarget <- getMostLikelyFactor(coldata, options) }
    positiveFactor <- ifelse(is.null(positiveFactor), positiveTarget, positiveFactor)
    logdebug(paste("    Converting factor data to binary factor with",positiveTarget,"->",positiveFactor))
    coldata <- as.factor(ifelse(coldata == positiveTarget, T, F))
    levels(coldata)[loc("TRUE",  levels(coldata))] <- positiveFactor
    levels(coldata)[loc("FALSE", levels(coldata))] <- paste("NOT",positiveFactor,sep="")
  }
  return( coldata )
}

convLogicaltoBinaryFactor <- function(coldata, positiveTarget = NULL, positiveFactor = NULL) {
  logdebug("  Converting logical column to binary factor type")

  cdata   <- class(coldata)
  if ( "logical" %in% cdata ) {
    if ( is.null(positiveTarget) ) { positiveTarget <- getMostLikelyFactor(coldata, options) }
    positiveFactor <- ifelse(is.null(positiveFactor), positiveTarget, positiveFactor)
    logdebug(paste("    Converting factor data to binary factor with",positiveTarget,"->",positiveFactor))
    coldata <- as.factor(ifelse(coldata == positiveTarget, T, F))
    levels(coldata)[loc("TRUE",  levels(coldata))] <- positiveFactor
    levels(coldata)[loc("FALSE", levels(coldata))] <- paste("NOT",positiveFactor,sep="")
  }
  return( coldata )
}

convNumerictoBinaryFactor <- function(coldata, positiveTarget = NULL, positiveFactor = NULL) {
  logdebug("  Converting numeric column to binary factor type")
  
  cdata   <- class(coldata)
  print(cdata)
  if ( any(c("numeric","integer") %in% cdata) ) {
    if ( is.null(positiveTarget) ) { positiveTarget <- getMostLikelyFactor(coldata, options) }
    positiveFactor <- ifelse(is.null(positiveFactor), positiveTarget, positiveFactor)
    logdebug(paste("    Converting numeric data to binary factor with",positiveTarget,"->",positiveFactor))
    coldata <- as.factor(ifelse(coldata == positiveTarget, T, F))
    levels(coldata)[loc("TRUE",  levels(coldata))] <- positiveFactor
    levels(coldata)[loc("FALSE", levels(coldata))] <- paste("NOT",positiveFactor,sep="")
  }
  return( coldata )
}







##############################################################################################################################
#
# Convert categorical data to numerical columns
#
##############################################################################################################################
expandFactorData <- function(cdata, target=NULL, positiveTarget=NULL, ignores=NULL) {
  loginfo(paste("  Converting categorical data for",ncol(cdata),"columns."))

  pdim    <- paste(dim(cdata), collapse = " x ")
  logdebug(paste("    Using positive target:",positiveTarget))

  repdf <- data.frame()
  for ( i in seq_along(colnames(cdata)) ) {
    colname <- colnames(cdata)[i]
    coldata <- cdata[,i]
    ctype   <- class(coldata)
    if ( "ordered" %in% ctype ) {
      coldata <- convOrderedToFactor(coldata)
      ctype   <- class(coldata)
    }
    
    logdebug(paste("    Converting",colname,"as class",ctype))
    if ( !is.null(ignores) ) {
      if ( colname %in% ignores ) {
        logdebug(paste("        Ignoring",colname))
        if ( ncol(repdf) == 0 ) { repdf <- data.frame(coldata) }
        else                    { repdf <- cbind(repdf, coldata) }
        colnames(repdf)[ncol(repdf)] <- colname
        next
      }
    }

    if ( ctype == "numeric" | ctype == "integer" | ctype == "Date" ) {
      logdebug(paste("    Passing col",colname,"because of class",ctype))
      if ( ncol(repdf) == 0 ) { repdf <- data.frame(coldata) }
      else                    { repdf <- cbind(repdf, coldata) }
      colnames(repdf)[ncol(repdf)] <- colname
    } else {
      logdebug(paste("    Converting categories of",colname,"because of class",ctype))
      convdata <- expandFactorColData(coldata, colname, positiveTarget)
      logdebug("=========> EXPAND")
      logdebug(colname)
      logdebug(colnames(convdata))
      if ( ncol(repdf) == 0 ) { repdf <- data.frame(convdata) }
      else                    { repdf <- cbind(repdf, convdata) }
    }
    
    if ( "coldata" %in% colnames(repdf) ) {
      print(colnames(repdf))
      stop()
    }
  }
  
  colnames(repdf) <- make.names(colnames(repdf))
  
  ndim <- paste(dim(repdf), collapse = " x ")
  loginfo(paste("  Previous Dim: ",pdim))
  loginfo(paste("   Convert Dim: ",ndim))
  return( repdf )
}


expandFactorColData <- function(coldata, colname, positiveTarget=NULL) {
  logdebug(paste("  Converting",class(coldata),"column",colname,"to numerical values."))
  
  if ( class(coldata) == "factor" ) {
    colclass <- "factor"
    uvals    <- levels(coldata)
    luvals   <- length(uvals)
  } else if ( class(coldata) == "logical" ) {
    colclass <- "logical"
    uvals    <- c(T,F)
    luvals   <- 2
  } else if ( class(coldata) == "character" ) {
    colclass <- "character"
    uvals    <- unique(coldata)
    luvals   <- length(uvals)
  } else {
    colclass <- "numeric"
    uvals    <- 0
    luvals   <- 0
  }
  
  if ( luvals == 0 ) {
    logdebug(paste("    Not altering",colname,"column."))
    return( coldata )
  }
  
  
  logdebug(paste("    Class/length of data is",class(coldata),",",luvals))
  
  ## Check for binary categories
  if ( luvals == 2 ) {
    if ( is.null(positiveTarget) ) { positiveTarget <- getMostLikelyFactor(coldata) }
    if ( positiveTarget %in% uvals ) {
      newcol   <- ifelse(coldata == positiveTarget, 1, 0)
      newcoldf <- data.frame(newcol)
      colnames(newcoldf) <- colname
      logdebug(paste("    Column",colname,"is now a numeric with 1,0 format."))
      return( newcoldf )
    } else {
      logerror(paste("Positive target",positiveTarget,"is not in list of values:", paste(uvals, collapse = ", ")))
    }
  }
  
  
  ## Convert to factor (if needed)
  if ( colclass == "character" | colclass == "logical" ) { coldata <- as.factor(coldata) }
  
  
  ## Split factors  
  newcoldf    <- data.frame("tmp"=coldata)
  newcolnames <- c("tmp")
  for ( uval in uvals ) {
    newcolname  <- make.names(paste(colname,uval,sep=""))
    newcol      <- ifelse(coldata == uval, 1, 0)
    newcoldf    <- cbind(newcoldf, as.integer(newcol))
    newcolnames <- c(newcolnames, newcolname)
  }
  colnames(newcoldf) <- newcolnames
  logdebug(paste("    Column",colname,"is now",ncol(newcoldf)-1,"columns with 1,0 format."))
  newcoldf <- newcoldf[,-1]
  return( newcoldf )
  
  logdebug(paste("    Not altering",colname,"column."))
  return( coldata )
}


