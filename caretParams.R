####################################################################################################################
#
# Get Caret Params
#
####################################################################################################################
getCaretDefaultParams <- function(classifier, traindata, targetcol = "TARGET", len = 1, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  pos <- loc(targetcol, colnames(traindata))
  if ( is.null(pos) ) { stop(paste("Could not find",targetcol,"in training data."))}
  trainX <- traindata[,-pos]
  trainY <- traindata[,pos]
  
  caretparams <- val <- getModelInfo(model = classifier, regex = F)
  if ( is.na(names(caretparams)) ) { stop(paste("No caret classifier named",classifier)) }
  caretparams <- caretparams[[classifier]]
  if ( is.null(caretparams) ) { stop(paste("No caret classifier data for classifier",classifier)) }
  
  params    <- NULL
  result = tryCatch({
    classgrid <- caretparams[["grid"]]
    params    <- classgrid(x = trainX, y = trainY, len = len)
    tmpparams <- classgrid(x = trainX, y = trainY, len = 1)
    if ( len > 1 ) {
      topparams <- head(params, n=len-1)
      params    <- rbind(tmpparams, topparams)
    } else {
      params    <- tmpparams
    }
  })
  
  return( params )
}


getTunedCaretDefaultParams <- function(classifier) {
  paramfilename = "caretparams.rData"
  ifile <- file.path(mvadir, "caret", paramfilename)
  load(ifile) ## returns tunes
  classifierTunes <- tunes[[classifier]]
  if ( !is.null(classifierTunes) ) {
    grid  <- classifierTunes[,"default"]
    fixed <- classifierTunes[,"fixed"]
  } else {
    grid  <- NULL
    fixed <- NULL
  }
  return( list("grid"=grid, "fixed"=fixed) )
}

  
getCaretParams <- function(classifier, traindata, targetcol = "TARGET", len = 1, useDefault = T, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  defparams   <- getTunedCaretDefaultParams(classifier)
  defgrid     <- defparams[["grid"]]
  deffixed    <- defparams[["fixed"]]
  if (is.null(defgrid)) {
    carparams   <- getCaretDefaultParams(classifier, traindata, targetcol, len = len)
  } else {
    carparams   <- getCaretDefaultParams(classifier, traindata, targetcol, len = len)
  }
  
  if (is.null(defgrid)) {
    if ( debug ) { writeLines("  Returning caret params because no custom grid") }
    grid <- carparams
  } else {
    if ( debug ) { writeLines("  Returning tuned caret params") }
    grid <- t(as.data.frame(defgrid))
    rownames(grid) <- seq(nrow(grid))
    grid <- as.data.frame(grid)
    if ( len > 1 ) {
      varRanges <- list()
      for ( var in names(grid) ) {
        isFixed <- deffixed[var]
        if ( isFixed ) {
          varRanges[[var]] <- grid[,var]
        } else {
          varRanges[[var]] <- unique(c(grid[,var], carparams[,var]))
        }
      }
      grid <- expand.grid(varRanges)
    }
  }

  if ( debug ) { 
    writeLines("  Using the following grid")
    print(grid)
  }

  return( grid )
}