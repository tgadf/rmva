mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "logger.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}


###########################################################################################
#
# MVA Performance
#
###########################################################################################
getMVAperformance <- function(testResults, truthLabels, positiveTarget = NULL, mva = NULL) {
  loginfo(paste("  Getting MVA performance metrics for MVA."))

  if ( is.null(testResults) ) {
    logwarn("MVA test results are all NULL. Returning with no performance curves.")
    return( NULL )
  }
  
  print(truthLabels)
  
  mvaScores <- testResults[["prob"]]
  mvaLabels <- testResults[["class"]]
  mvaValues <- testResults[["value"]]

  problemType <- getProblemType(truthLabels)

  if ( problemType == "cluster" ) {
    loginfo("    Getting performance metrics assuming this was a clustering problem.")
    logdebug("  Getting Output")
    output <- getClusterOutput(mvaScores = mvaValues)
    
    isGood <- T
    if ( isGood ) { logdebug(paste("    Model is corrected trained and tested."))}
    else          { logdebug(paste("    Model had problems during training or testing."))}
    
    retval <- list("output"=output, "isGood"=isGood)
    return( retval )
  }
  
  if ( problemType == "linearRegression" ) {
    loginfo("    Getting performance metrics assuming this was a linear regression problem.")
    # SMRE and MAE
    logdebug("  Getting RMSE and MAE")
    errors <- getResidualErrors(mvaScores = mvaValues, truthScores = truthLabels)

    # Rel Output
    logdebug("  Getting Residuals")
    residuals <- getResiduals(mvaScores = mvaValues, truthScores = truthLabels)

    # Scores
    logdebug("  Getting Output")
    output <- getRegressionOutput(mvaScores = mvaValues, truthLabels)
    
    #
    logdebug("  Getting Status")
    isGood <- getStatus(mvaScores = mvaValues, mva = mva)
    isGood <- T
    if ( isGood ) { logdebug(paste("    Model is corrected trained and tested."))}
    else          { logdebug(paste("    Model had problems during training or testing."))}
    
    retval <- list("errors"=errors, "residuals"=residuals, "output"=output, "isGood"=isGood)
    return( retval )
  }
  
  if ( problemType == "binaryClassification" ) {
    loginfo("    Getting performance metrics assuming this was a binary classification problem.")
    # Lift
    logdebug("  Getting Lift")
    lift <- getLift(mvaScores, truthLabels, positiveTarget, options, nquants = 25)
    
    # Confusion
    logdebug("  Getting Confusion Matrix")
    conf <- getConfusionMatrix(mvaLabels, truthLabels, positiveTarget)
    
    # ROC
    logdebug("  Getting ROC")
    roc <- getROC(mvaScores, truthLabels, positiveTarget)
    tpr <- roc[["tpr"]]
    fpr <- roc[["fpr"]]
    pts <- roc[["pts"]]
    auc <- roc[["auc"]]
    
    # Scores
    logdebug("  Getting Output")
    output <- getOutput(mvaScores, truthLabels, positiveTarget)
    
    #
    logdebug("  Getting Status")
    isGood <- getStatus(mvaScores, mva = mva)
    if ( isGood ) { logdebug(paste("    Model is corrected trained and tested."))}
    else          { logdebug(paste("    Model had problems during training or testing."))}
    
    retval <- list("lift"=lift, "confusion"=conf, "tpr"=tpr, "fpr"=fpr, "auc"=auc, "output"=output, "isGood"=isGood)
    return( retval )
  }
  
  
  print(mvaScores)
  print(mvaLabels)
  print(mvaValues)
  stop("Error with input to getMVAPerformance()")
}


###########################################################################################
#
# Status Functions
#
###########################################################################################
getStatus <- function(mvaScores, mva = NULL) {
  logdebug(paste("Getting status for MVA",mva,"fit."))

  nUniqueValues <- getUniqueValues(mvaScores, sampleSize = floor(length(mvaScores)/3))
  if ( var(mvaScores) <= 0 ) {
    logwarn(paste("MVA",mva,"output has zero variance so the fit is likely no good."))
    return( F )
  }
  if ( nUniqueValues < floor(length(mvaScores)/50) ) {
    logwarn(paste("MVA",mva,"has",nUniqueValues,"unique values and needs at least",floor(length(mvaScores)/50)))
    return( F )
  }
  return( T )
}


###########################################################################################
#
# Lift Functions
#
###########################################################################################
getQuantiles      <- function(scores, nbins) {
  if ( any(is.na(scores)) ) { return( NULL ) }
  dq <- 1/nbins
  return( quantile(scores, probs = seq(0, 1-dq, dq)) )
}

getQuantileBins     <- function(scores, quantiles) {
  if ( any(is.na(scores)) )    { return( NULL ) }
  if ( any(is.na(quantiles)) ) { return( NULL ) }
  if ( is.null(quantiles) )    { return( NULL ) }
  
  quantilebins <- sapply(quantiles, function(x) max(which(scores >= x)))
  isInf <- which(is.infinite(quantilebins))
  while ( length(isInf) > 0 ) {
    isInf <- which(is.infinite(quantilebins))
    for ( pos in isInf ) {
      if ( pos == 1 ) { quantilebins[pos] <- quantilebins[pos+1] }
      else if ( pos == length(quantilebins) ) { quantilebins[pos] <- quantilebins[pos-1] }
      else { quantilebins[pos] = quantilebins[pos-1] }
    }
  }
  return( rev(quantilebins) )
}

getGainByQuantile <- function(quantilebins, truthLabels) {
  truePositives <- sapply(quantilebins, function(x) sum(truthLabels[1:x]))
  gain          <- 100 * truePositives / max(truePositives)
  return( gain )
}

getCaptureByQuantile <- function(quantilebins, truthLabels) {
  capture <- getGainByQuantile(quantilebins, truthLabels) / 100
  return( capture )
}

getCumulativeLiftByQuantile <- function(gain, nbins) {
  fracData       <- seq(100/nbins, 100, 100/nbins)
  cumulativeLift <- gain / fracData
  return( cumulativeLift )
}

orderScores <- function(mvaScores, truthLabels, positiveTarget) {
  if ( "factor" %in% class(truthLabels) ) {
    truthLabels <- convFactortoBinary(truthLabels, positiveTarget = positiveTarget)
  }
  orderedData <- cbind(mvaScores, truthLabels)
  orderedData <- orderedData[order(-orderedData[, 1]), ]
  return( orderedData )
}

getCapture <- function(mvaScores, truthLabels, positiveTarget, nquants = 10) {
  logdebug("  Getting MVA lift values.")
  
  liftData    <- orderScores(mvaScores = mvaScores, truthLabels = truthLabels, positiveTarget = positiveTarget)
  
  scores      <- liftData[,1]
  truthLabels <- liftData[,2]
  quantiles      <- getQuantiles(scores, nquants)
  if ( is.null(quantiles) ) { return( NULL ) }
  quantilebins   <- getQuantileBins(scores = scores, quantiles = quantiles)
  capture        <- getCaptureByQuantile(quantilebins = quantilebins, truthLabels = truthLabels)
  
  return( round(capture, digits = 3) )
}

getLift <- function (mvaScores, truthLabels, positiveTarget, nquants = 10, ...) {
  options        <- unlist(list(...))
  debug          <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  logdebug("  Getting MVA lift values.")
  
  liftData    <- orderScores(mvaScores = mvaScores, truthLabels = truthLabels, positiveTarget = positiveTarget)
  
  scores      <- liftData[,1]
  truthLabels <- liftData[,2]
  
  quantiles      <- getQuantiles(scores, nquants)
  if ( is.null(quantiles) ) { return( NULL ) }
  quantilebins   <- getQuantileBins(scores = scores, quantiles = quantiles)
  gain           <- getGainByQuantile(quantilebins = quantilebins, truthLabels = truthLabels)
  cumulativeLift <- getCumulativeLiftByQuantile(gain = gain, nbins = nquants)
  
  return( round(cumulativeLift, digits = 3) )
}



###########################################################################################
#
# Error Functions
#
###########################################################################################
rmse <- function(error)
{
  # Function that returns Root Mean Squared Error
  sqrt(mean(error^2))
}

mae <- function(error)
{
  # Function that returns Mean Absolute Error
  mean(abs(error))
}

getResidualErrors <- function (mvaScores, truthScores, ...) {
  options        <- unlist(list(...))
  debug          <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  logdebug(paste("  Getting RMSE and MAE for",length(mvaScores),"entries."))
  
  dval    <- mvaScores - truthScores
  valRMSE <- rmse(dval)
  valMAE  <- mae(dval)
  return( list("rmse"=valRMSE, "mae"=valMAE) )
}

getRegRelQuantiles <- function(mvaScores, truthScores, ...) {
  options        <- unlist(list(...))
  debug          <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  logdebug(paste("  Getting relative difference quantiles for",length(mvaScores),"entries."))
  
  dval  <- mvaScores - truthScores
  erf   <- function(x) 2 * pnorm(x * sqrt(2)) - 1
  sigs  <- sapply(seq(1, 3), function(x) erf(x/sqrt(2)))
  sigs  <- c(rev(1-sigs), sigs)
  qvals <- quantile(dval, probs = sigs)
  names(qvals) <- paste(c(seq(-3,-1),seq(1,3)),"sigma",sep = "")
  return( round(qvals,3) )
}

capOutputByQuantile <- function(input, quant) {
  maxVal     <- max(abs(quantile(input, probs = c(min(quant, 1-quant), max(quant, 1-quant)))))
  maxDVals   <- ifelse(input < -1*maxVal, -1*maxVal, input)
  maxDVals   <- ifelse(maxDVals > maxVal, maxVal, maxDVals)
  return( maxDVals )
}

getResiduals <- function (mvaScores, truthScores) {
  require(MASS)
  
  logdebug(paste("  Getting relative difference for",length(mvaScores),"entries."))
  
  dval       <- mvaScores - truthScores
  residuals  <- capOutputByQuantile(dval, 0.02)
  maxVal     <- round(max(abs(residuals))+0.1, 1)
  if ( length(residuals) > 300 ) {
    tmp        <- hist(residuals, breaks = seq(-1*maxVal, maxVal, length.out = 150), plot = F)
  } else if ( length(residuals) > 100 ) {
    tmp        <- hist(residuals, breaks = seq(-1*maxVal, maxVal, length.out = 25), plot = F)
  } else if ( length(residuals) > 50 ) {
    tmp        <- hist(residuals, breaks = seq(-1*maxVal, maxVal, length.out = 10), plot = F)
  } else {
    tmp        <- hist(residuals, breaks = seq(-1*maxVal, maxVal, length.out = 5), plot = F)
  }
  counts     <- tmp[["counts"]]
  pts        <- tmp[["mids"]]
  sigmacuts  <- getRegRelQuantiles(mvaScores, truthScores)
  
  fd      <- fitdistr(residuals, "normal")

  return( list("counts"=counts, "pts"=pts, "cuts"=sigmacuts, "fit"=fd) )
}

getRegressionOutput <- function (mvaScores, truthScores) {
  logdebug(paste("  Getting regression output for",length(mvaScores),"entries."))

  library(RColorBrewer)
  rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r <- rf(32)

  isGood <- ifelse(var(mvaScores) == 0, F, T)
  if ( isGood & F) {
  require(hexbin)
  capVal <- quantile(c(truthScores, mvaScores), probs = 0.99)
  df <- data.frame("truth"=ifelse(truthScores > capVal, capVal, truthScores), "prediction"=ifelse(mvaScores > capVal, capVal, mvaScores))
  h  <- hexbinplot(prediction~truth, data=df, colramp=rf, mincnt=1, trans=log, inv=exp, xlim = c(min(df), max(df)), ylim = c(min(df), max(df)))
  } else { h <- NULL }

  if ( isGood ) {  
    tmp <- hist(c(mvaScores, truthScores), breaks = 50, plot = F)
    posHist <- hist(truthScores, breaks = tmp[["breaks"]], plot = F)
    negHist <- hist(mvaScores, breaks = tmp[["breaks"]], plot = F)
    posCounts <- posHist[["counts"]]
    negCounts <- negHist[["counts"]]
    pts       <- tmp[["mids"]]
    linout <- list("truthCounts"=posCounts, "scoreCounts"=negCounts, "pts"=pts)
  } else { linout <- list("truthCounts"=NULL, "scoreCounts"=NULL, "pts"=NULL)  }

  if ( isGood ) {  
    tmp <- hist(c(log1p(mvaScores), log1p(truthScores)), breaks = 50, plot = F)
    posHist <- hist(log1p(truthScores), breaks = tmp[["breaks"]], plot = F)
    negHist <- hist(log1p(mvaScores), breaks = tmp[["breaks"]], plot = F)
    posCounts <- posHist[["counts"]]
    negCounts <- negHist[["counts"]]
    pts       <- tmp[["mids"]]
    logout <- list("truthCounts"=posCounts, "scoreCounts"=negCounts, "pts"=pts)
  } else { logout <- list("truthCounts"=NULL, "scoreCounts"=NULL, "pts"=NULL)  }
  
  if ( length(mvaScores) < 10000 ) {
    values <- list("value"=mvaScores, "truth"=truthScores)
  } else {
    keep <- sample(seq(length(mvaScores)), size = 10000)
    values <- list("value"=mvaScores[keep], "truth"=truthScores[keep])
  }

  return( list("hist2d"=h, "linear"=linout, "log"=logout, "values"=values) )
}




##########################################################################################################
#
# Cluster Output
#
##########################################################################################################
getClusterOutput <- function (mvaScores) {
  logdebug(paste("  Getting regression output for",length(mvaScores),"entries."))

  if ( length(mvaScores) < 10000 ) {
    values <- list("value"=mvaScores)
  } else {
    keep <- sample(seq(length(mvaScores)), size = 10000)
    values <- list("value"=mvaScores[keep])
  }
  
  return( list("values"=values) )
}



##########################################################################################################
#
# Confusion Matrix
#
##########################################################################################################
getConfusionMatrix <- function(mvaLabels, truthLabels, positiveTarget) {
  logdebug("  Getting MVA confusion matrix.")
  require(caret)
  
  return( NULL )
  confMatrix <- caret::confusionMatrix(mvaLabels, truthLabels, positive = as.character(positiveTarget))
  return( confMatrix )
}


##########################################################################################################
#
# Performace
#
##########################################################################################################
getROC <- function(mvaScores, truthLabels, positiveTarget) {
  logdebug("  Getting true-positive/false-positive rates.")
  
  if ( any(is.null(mvaScores)) ) {
    logwarn("    Found a NULL in MVA scores. Returning NULL.")
    return( NULL )
  }
  if ( any(is.na(mvaScores)) ) {
    logwarn("    Found an NA in MVA scores. Returning NULL.")
    return( NULL )
  }
  
  retvals <- computeTruePositiveFalsePositiveRates(mvaScores, truthLabels, positiveTarget, ncuts = 100)
  return( retvals )
}

computeTruePositiveFalsePositiveRates <- function(mvaScores, truthLabels, positiveTarget, ncuts=100) {
  logdebug(paste("  Compute True/False positive for",length(mvaScores),"MVA scores."))
  
  cuts <- sort(unique(quantile(round(mvaScores,2), probs = seq(0, 1, 1/ncuts))), decreasing = T)
  logdebug(paste("    Using",length(cuts),"cut points for True/False positive rates."))
  Ntp  <- length(truthLabels[truthLabels == positiveTarget])
  Nfp  <- length(truthLabels[truthLabels != positiveTarget])
  tpr   <- c(0.0)
  fpr   <- c(0.0)
  pts   <- c(1.0)
  for ( cut in cuts ) {
    p <- (which(mvaScores >= cut))
    pr <- mvaScores[p]
    cl <- truthLabels[p]
    tp <- round(length(cl[cl == positiveTarget]) / Ntp, 2)
    fp <- round(length(cl[cl != positiveTarget]) / Nfp, 2)
    tpr   <- c(tpr, tp)
    fpr   <- c(fpr, fp)
    pts   <- c(pts, cut)
    #print(paste("TP:",cut,tp,fp,tpcut[1]))
  }
  tpr   <- c(tpr, 1.0)
  fpr   <- c(fpr, 1.0)
  pts   <- c(pts, 0.0)
  auc   <- getAUC(tpr, fpr)
  return( list("tpr"=tpr, "fpr"=fpr, "pts"=pts, "auc"=auc) )
}


getAUC <- function(tpr, fpr) {
  if ( require(zoo) ) {
    id <- order(tpr)
    rAUC <- sum(diff(fpr[id])*rollmean(tpr[id],2))  
    return( rAUC )
  } else {
    raise("No zoo!")
  }
}




###########################################################################################
#
# Output Functions
#
###########################################################################################
getOutput <- function(mvaScores, truthLabels, positiveTarget) {
  logdebug("  Getting output values.") 
  
  pos <- which(truthLabels == positiveTarget)
  posOutput <- mvaScores[pos]
  neg <- which(truthLabels != positiveTarget)
  negOutput <- mvaScores[neg]
  
  tmp <- hist(mvaScores, breaks = 50, plot = F)
  posHist <- hist(posOutput, breaks = tmp[["breaks"]], plot = F)
  negHist <- hist(negOutput, breaks = tmp[["breaks"]], plot = F)
  posCounts <- posHist[["counts"]]
  negCounts <- negHist[["counts"]]
  pts       <- tmp[["mids"]]
  
  return( list("posCounts"=posCounts, "negCounts"=negCounts, "pts"=pts) )
}