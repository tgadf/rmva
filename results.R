mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "logger.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}

##########################################################################################
#
# Plotting Functions
#
###########################################################################################
getCols <- function(i = NULL) {
  col1 <- '#34314C' # navy
  col2 <- '#59CD90' # light green
  col3 <- '#EE6352' # red
  col4 <- '#3FA7D6' # light blue
  col5 <- '#FAC05E' # gold
  cols <- c(col3, col4, col1, col2, col5)
  if ( is.null(i) ) { return( cols ) }
  else              { 
    if ( i >= 1 & i <= 5 ) { return( cols[i] ) }
    else                   { i <- i %% 5; return( cols[i] ) }
  }
}

getVectorEntries <- function(svals, val) {
  keep <- sapply(svals, function(x) x[[val]])
  if ( class(keep) == "list" ) { keep <- unlist(keep) }
  return( keep )
}

getListEntries <- function(lvals, val) {
  keep   <- lapply(lvals, function(x) x[[val]])
  isNULL <- sapply(keep, function(x) is.null(x))
  keep   <- keep[!isNULL]
  return( keep )
}


setMargins <- function(l=1, r=0.5, b=1, t=0.75) {        
  leftmar   <- l
  rightmar  <- r
  bottommar <- b
  topmar    <- t
  par(mai=c(bottommar,leftmar,topmar,rightmar))
}

addLegend <- function(...) {
  par(mar = c(5, 4, 1.4, 0.2))
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}


plotResults <- function(mvaPerformances, basename = "test") {
  isGood    <- getVectorEntries(mvaPerformances, "isGood")
  residuals <- getListEntries(mvaPerformances, "residuals")
  output    <- getListEntries(mvaPerformances, "output")
  print(residuals)
  if ( is.null(residuals) | length(residuals) == 0 ) {
    if ( is.null(output) | length(output) == 0 ) {
      problemType <- "cluster"
    } else {
      problemType <- "binaryClassification"
    }
  } else {
    problemType <- "linearRegression"
  }

  loginfo("\n--------- Plotting Results ----------")
  loginfo(paste("     Type:",problemType))
  loginfo(paste("   Output:",basename))
  loginfo(paste("    Ngood:",length(isGood)))

  if ( length(isGood) == 0 ) {
    logwarn("There are no good MVAs in this list. Returning without plots.")
    return(NULL)
  }
  
  if ( problemType == "linearRegression" ) {
    loginfo("  Plotting Residuals.")
    residuals <- getListEntries(mvaPerformances, "residuals")
    counts    <- getListEntries(residuals, "counts")
    pts       <- getListEntries(residuals, "pts")
    sigmacuts <- getListEntries(residuals, "cuts")
    normfits  <- getListEntries(residuals, "fit")
    errors <- getListEntries(mvaPerformances, "errors")
    maes   <- getVectorEntries(errors, "mae")
    rmses  <- getVectorEntries(errors, "rmse")
    output         <- getListEntries(mvaPerformances, "output")
    values         <- getListEntries(output, "values")
    
    #plotResidualHist(counts, pts, sigmacuts, maes, rmses, isGood, basename)
    if ( !is.null(values) ) {
      value <- getListEntries(values, "value")
      truth <- getListEntries(values, "truth")
      plotResiduals(value, truth, normfits, maes, rmses, isGood, basename = basename)
    }
    
    loginfo("  Plotting MAEs.")
    plotMAE(maes, ppp = 20, basename)
    
    loginfo("  Plotting RMSEs.")
    plotRMSE(rmses, ppp = 20, basename)
    
    loginfo("  Plotting FitVals")
    plotFitVals(normfits, ppp = 20, basename)
    
    ## Output
    loginfo("  Plotting output.")
    if ( !is.null(values) ) {
      value <- getListEntries(values, "value")
      truth <- getListEntries(values, "truth")
      plotTruth(value, truth, maes, rmses, isGood, basename = basename)
      plotTruth2D(value, truth, maes, rmses, isGood, basename = basename)
    }
    linout         <- getListEntries(output, "linear")
    truthCountsLin <- getListEntries(linout, "truthCounts")
    scoreCountsLin <- getListEntries(linout, "scoreCounts")
    ptsLin         <- getListEntries(linout, "pts")
    plotValue(truthCountsLin, scoreCountsLin, ptsLin, maes, rmses, isGood, basename = basename, isLog = F)
    logout         <- getListEntries(output, "log")
    truthCountsLog <- getListEntries(logout, "truthCounts")
    scoreCountsLog <- getListEntries(logout, "scoreCounts")
    ptsLog         <- getListEntries(logout, "pts")
    plotValue(truthCountsLog, scoreCountsLog, ptsLog, maes, rmses, isGood, basename = basename, isLog = T)
  }
  
  if ( problemType == "binaryClassification" ) {
    ## Only plot Good Guys
    
    
    ## Lifts
    loginfo("  Plotting Lift.")
    lifts <- getListEntries(mvaPerformances, "lift")
    if ( !any(sapply(lifts, function(x) is.null(x))) ) {
      plotLift(lifts = lifts, isGood, ppp = 8, basename = basename, options)
    } else {
      loginfo("Found a NULL in Lift. Not plotting...")
    }
    
    ## ROCs
    loginfo("  Plotting ROC.")
    tprs <- getListEntries(mvaPerformances, "tpr")
    fprs <- getListEntries(mvaPerformances, "fpr")
    aucs <- getListEntries(mvaPerformances, "auc")
    if ( !any(sapply(aucs, function(x) is.null(x))) ) {
      plotROC(tprs = tprs, fprs = fprs, aucs = aucs, isGood, ppp = 8, basename = basename, options)
    } else {
      loginfo("AUC is NULL. Not plotting...")
    }
    
    ## Accuracy
    loginfo("  Plotting Accuracy.")
    conf    <- getListEntries(mvaPerformances, "confusion")
    overall <- getListEntries(conf, "overall")
    accuracy     <- getVectorEntries(overall, "Accuracy")
    accuracyP1   <- getVectorEntries(overall, "AccuracyUpper")
    accuracyM1   <- getVectorEntries(overall, "AccuracyLower")
    if ( !any(sapply(accuracy, function(x) is.null(x))) ) {
      plotAccuracy(accVals = accuracy, accP1Vals = accuracyP1, accM1Vals = accuracyM1, isGood,
                   ppp = 20, basename = basename, options)
    } else {
      loginfo("Accuracy is NULL. Not plotting...")
    }
    
    ## Kappa
    loginfo("  Plotting Kappa.")
    kappa     <- getVectorEntries(overall, "Kappa")
    if ( !any(sapply(kappa, function(x) is.null(x))) ) {
      plotKappa(kappaVals = kappa, isGood, basename = basename, options)
    } else {
      loginfo("Kappa is NULL. Not plotting...")
    }
    
    ## pValues
    loginfo("  Plotting p-Value.")
    pValue     <- getVectorEntries(overall, "McnemarPValue")
    if ( !any(sapply(pValue, function(x) is.null(x))) ) {
      plotpValue(pVals = pValue, isGood, basename = basename, options)
    } else {
      loginfo("p-Value is NULL. Not plotting...")
    }
    
    ## Output
    if ( debug ) { loginfo("  Plotting output.") }
    output    <- getListEntries(mvaPerformances, "output")
    posCounts <- getListEntries(output, "posCounts")
    negCounts <- getListEntries(output, "negCounts")
    pts       <- getListEntries(output, "pts")
    if ( !any(sapply(pValue, function(x) is.null(x))) ) {
      plotOutput(posCounts, negCounts, pts, aucs, lifts, isGood, basename = basename, options)
    } else {
      loginfo("p-Value is NULL. Not plotting...")
    }
  }
}



##############################################################################################################################
#
# Lift Curves
#
##############################################################################################################################
plotLift <- function(lifts, isGood, ppp, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  require(RColorBrewer)
  
  lifts[isGood == F] <- NULL
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"lift","pdf",sep="."))
  loginfo(paste("Plotting Lift for",length(lifts),"classifiers."))
  loginfo(paste("  Saving all plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  items          <- sortAndGroupItems(lifts, ppp)
  loginfo(paste("  Writing",length(items),"Lift pages."))
  
  for ( item in items ) {
    nplots   <- length(item)
    cols     <- rep(brewer.pal(ppp,"Dark2"), length.out = nplots)
    title    <- "Classifier Lift"
    legnames <- c()
    for ( i in seq(item) ) {
      classifier <- item[[i]]
      lift       <- lifts[[classifier]]
      if ( debug ) { loginfo(paste("    ",classifier," - N =",length(lift)," - Max =",max(lift))) }
      if ( !is.finite(max(lift)) ) {
        loginfo(paste("  ===> Lift for",classifier,"has problems."))
        return(NULL)
      }
      if ( i == 1 ) {
        maxLift  <- max(lift)
        ymax     <- as.integer(max(unlist(maxLift))+1)
        plot(lift, type='c', lwd=3, cex=0.25, col = cols[i], las=2,
             ylim = c(0, ymax), axes=F, xlab="Percentile", ylab="Lift by Percentile", main=title)
        axis(1, at=seq(lift), labels = names(lift))
        axis(2, at=seq(0, ymax), labels = seq(0, ymax))
        points(lift, col=cols[i], pch=19)
        grid(10, 20)
      } else {
        par(new=T)
        plot(lift, type='c', lwd=3, cex=0.25, col = cols[i], las=2,
             ylim = c(0, ymax), axes=F, xlab="", ylab="")
        points(lift, col=cols[i], pch=19)
      }
      legname <- paste(signif(max(lift),2), "-", classifier)
      if ( debug ) { loginfo(paste("  ",legname)) }
      legnames <- c(legnames, legname)
    }
    box()
    legend("topright", inset=.05, title="Classifier Lift (w/ max)",
           legnames, col = cols, fill = cols, horiz = F)
  }
  
  dev.off()
  loginfo(paste("Saved all plots to",pdfname))
}



##############################################################################################################################
#
# ROC Curves
#
##############################################################################################################################
plotROC <- function(tprs, fprs, aucs, isGood, ppp, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  require(RColorBrewer)
  
  tprs[isGood == F] <- NULL
  fprs[isGood == F] <- NULL
  aucs[isGood == F] <- NULL
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"roc","pdf",sep="."))
  loginfo(paste("Plotting ROC for",length(aucs),"classifiers."))
  loginfo(paste("  Saving all ROC plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  logScale <- ifelse ("log" %in% options, T, F)
  if ( logScale ) { loginfo("  Using Log Scale.") }
  
  items <- sortAndGroupItems(aucs, ppp)
  loginfo(paste("  Writing",length(items),"ROC pages."))
  
  for ( item in items ) {
    nplots   <- length(item)
    cols     <- rep(brewer.pal(ppp,"Dark2"), length.out = nplots)
    title    <- "Classifier ROC"
    legnames <- c()
    ltys     <- replicate(nplots, 1)
    if ( logScale ) {
      ylim <- c(0.01, 1.01)
      xlim <- c(0.01, 1.01)
    } else {
      ylim <- c(-0.01, 1.01)
      xlim <- c(-0.01, 1.01)
    }
    
    for ( i in seq(item) ) {
      classifier <- item[i]
      if ( !isGood[classifier] ) { next }
      tpr        <- tprs[[classifier]]
      fpr        <- fprs[[classifier]]
      auc        <- aucs[[classifier]]
      if ( i == 1 ) {
        plot(tpr~fpr, type='l', lwd=2, lty=ltys[i], cex=0.25, col = cols[i], log = '',
             ylim = ylim, xlim = xlim, axes=T, xlab="False Positive Rate", ylab="True Positive Rate", main=title)
        grid()
      } else {
        par(new=T)
        plot(tpr~fpr, type='l', lwd=2, lty=ltys[i], cex=0.25, col = cols[i], log = '',
             ylim = ylim, xlim = xlim, axes=F, xlab="", ylab="")
      }
      legname <- paste(signif(auc,3), "-", classifier)
      if ( debug ) { loginfo(paste("  ",legname)) }
      legnames <- c(legnames, legname)
    }
    box()
    legend("bottomright", inset=.05, title="Model Performance",
           legnames, col = cols, fill = cols, horiz = F)
  }
  
  dev.off()
  loginfo(paste("Saved all plots to",pdfname)) 
}



##############################################################################################################################
#
# Accuracy Curves
#
##############################################################################################################################
plotAccuracy <- function(accVals, accP1Vals, accM1Vals, isGood, ppp, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  require(RColorBrewer)
  
  accVals   <- accVals[isGood]
  accP1Vals <- accP1Vals[isGood]
  accM1Vals <- accM1Vals[isGood]
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"acc","pdf",sep="."))
  loginfo(paste("  Plotting accuracy for",length(accVals),"classifiers."))
  loginfo(paste("    Saving all accuracy plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  items <- sortAndGroupItems(accVals, ppp)
  loginfo(paste("  Writing",length(items),"accuracy pages."))
  
  setMargins(l = 2)  
  for ( item in items ) {
    nplots   <- length(item)
    #cols     <- rep(brewer.pal(ppp,"Dark2"), length.out = nplots)
    cols     <- topo.colors(n = nplots)
    title    <- "Classifier ROC"
    legnames <- c()
    ltys     <- replicate(nplots, 1)
    
    accuracy      <- accVals[item]
    accuracyLower <- accM1Vals[item]
    accuracyUpper <- accP1Vals[item]
    df <- data.frame(accuracy, accuracyLower, accuracyUpper)
    df[,"sd"] <- (accuracyUpper + accuracyLower)/2
    df[,"ID"] <- names(accuracy)
    df <- df[order(accuracy),]
    
    accVal <- df[,"accuracy"]
    dotchart(accVal, col="blue", xlim=c(0.45, 1.05))
    for (i in 1:nrow(df)){
      lines(x=c(df[i,"accuracyLower"],df[i,"accuracyUpper"]), y=c(i,i))
    }
    axis(2, at = seq(nrow(df)), labels = df[,"ID"], las=2)
  }
  
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}






##############################################################################################################################
#
# Kappa Curves
#
##############################################################################################################################
plotKappa <- function(kappaVals, isGood, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  require(RColorBrewer)

  kappaVals <- kappaVals[isGood]
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"kappa","pdf",sep="."))
  loginfo(paste("  Plotting kappa for",length(kappaVals),"classifiers."))
  loginfo(paste("    Saving all kappa plots to",pdfname))
  
  pdf(pdfname)
  setMargins(b = 2.5)
  
  
  cols <- replicate(n = length(kappaVals), expr = "dodgerblue")
  pchs <- replicate(n = length(kappaVals), expr = 19)
  cols[kappaVals < 0.5] <- "darkorange"
  pchs[kappaVals < 0.5] <- 15
  cols[kappaVals < 0.25] <- "darkred"
  pchs[kappaVals < 0.25] <- 17
  plot(kappaVals, ylim=c(-0.01, 1.01), col=cols, pch=pchs, axes=F, xlab="", ylab="Kappa Values")
  axis(1, at=seq(length(kappaVals)), labels = names(kappaVals), las=2, cex=0.75)
  axis(2)
  box()
  grid()
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}




##############################################################################################################################
#
# pValues Curves
#
##############################################################################################################################
plotpValue <- function(pVals, isGood, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  require(RColorBrewer)
  
  pVals <- pVals[isGood]
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"pval","pdf",sep="."))
  loginfo(paste("  Plotting p-Values for",length(pVals),"classifiers."))
  loginfo(paste("    Saving all p-value plots to",pdfname))
  
  pdf(pdfname)
  setMargins(b = 2.5)
  
  cols <- replicate(n = length(pVals), expr = "dodgerblue")
  pchs <- replicate(n = length(pVals), expr = 19)
  cols[pVals < 5e-2] <- "darkorange"
  pchs[pVals < 5e-2] <- 15
  cols[pVals < 1e-5] <- "darkred"
  pchs[pVals < 1e-5] <- 17
  plot(log(pVals), col=cols, pch=pchs, axes=F, xlab="", ylab="p-Values")
  axis(1, at=seq(length(pVals)), labels = names(pVals), las=2, cex=0.75)
  axis(2)
  box()
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}



##############################################################################################################################
#
# Output Curves
#
##############################################################################################################################
plotOutput <- function(posCounts, negCounts, pts, aucs, lifts, isGood, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  require(RColorBrewer)
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"output","pdf",sep="."))
  loginfo(paste("  Plotting output for",length(pts),"classifiers."))
  loginfo(paste("    Saving all output plots to",pdfname))
  
  pdf(pdfname)
  setMargins()

  classifiers <- names(pts)
  for ( classifier in classifiers ) {
    posOutput <- posCounts[[classifier]]
    negOutput <- negCounts[[classifier]]
    xValues   <- pts[[classifier]]
    auc       <- aucs[[classifier]]
    lift      <- max(lifts[[classifier]])
    good      <- isGood[[classifier]]

    ymax <- max(posOutput, negOutput)
    title <- paste(classifier,"Output")
    info <- c()
    info <- paste(info, "  isGood:",as.character(good))
    info <- paste(info, "  AUC:",signif(auc,3))
    info <- paste(info, "  Max(Lift):",signif(lift,3))
    title <- paste(title, info, sep="\n")
    loginfo(title)
    plot(posOutput~xValues, type='b', lwd=2, lty=1, col = 'dodgerblue', log = '', pch=19,
           ylim = c(0, ymax*1.25), axes=T, xlab="Output", ylab="Density", main=title)
    grid()
    par(new=T)
    plot(negOutput~xValues, type='b', lwd=2, lty=1, col = 'darkorange', log = '', pch=17,
         ylim = c(0, ymax*1.25), axes=F, xlab="", ylab="", main="")
    
    cols <- c('dodgerblue', 'darkorange')
    legend("top", inset=.05, title="Classifier Output",
           c("Positive", "Negative"), col = cols, fill = cols, horiz = F)    
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}




##############################################################################################################################
#
# Output Values
#
##############################################################################################################################
plotValue <- function(truthCounts, scoreCounts, pts, maes, rmses, isGood, basename, isLog = F) {
  require(RColorBrewer)
  
  if ( isLog ) { pdfname <- file.path(mvadir, "plots", paste(basename,"logvalue","pdf",sep=".")) }
  else         { pdfname <- file.path(mvadir, "plots", paste(basename,"value","pdf",sep=".")) }

  loginfo(paste("  Plotting output for",length(pts),"mvas"))
  loginfo(paste("    Saving all output plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  classifiers <- names(pts)
  for ( classifier in classifiers ) {
    truthOutput <- truthCounts[[classifier]]
    scoreOutput <- scoreCounts[[classifier]]
    xValues     <- pts[[classifier]]
    mae         <- maes[[classifier]]
    rmse        <- rmses[[classifier]]
    good        <- isGood[[classifier]]
    
    ymax <- max(truthOutput, scoreOutput)
    title <- paste(classifier,"Response")
    info <- c()
    info <- paste(info, "  isGood:",as.character(good))
    info <- paste(info, "  MAE:",signif(mae,3))
    info <- paste(info, "  RMSE:",signif(rmse,3))
    title <- paste(title, info, sep="\n")
    
    par(mfrow=c(2,1))
    plot(truthOutput~xValues, type='b', lwd=2, lty=1, col = 'dodgerblue', log = '', pch=19,
         ylim = c(0, ymax*1.25), axes=T, xlab="Response", ylab="Density", main=title)
    grid()
    par(new=T)
    plot(scoreOutput~xValues, type='b', lwd=2, lty=1, col = 'darkorange', log = '', pch=17,
         ylim = c(0, ymax*1.25), axes=F, xlab="", ylab="", main="")
    
    cols <- c('dodgerblue', 'darkorange')
    legend("top", inset=.05, title="Classifier Response",
           c("Truth", "Score"), col = cols, fill = cols, horiz = F)    
    
    plot(truthOutput~xValues, type='b', lwd=2, lty=1, col = 'dodgerblue', log = 'xy', pch=19,
         ylim = c(1, ymax*3), axes=T, xlab="Response", ylab="Density", main="")
    grid()
    par(new=T)
    plot(scoreOutput~xValues, type='b', lwd=2, lty=1, col = 'darkorange', log = 'xy', pch=17,
         ylim = c(1, ymax*3), axes=F, xlab="", ylab="", main="")
    
    cols <- c('dodgerblue', 'darkorange')
    #legend("top", inset=.05, title="Classifier Response",
    #       c("Truth", "Score"), col = cols, fill = cols, horiz = F)    
    
    
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}




##############################################################################################################################
#
# Output Values (if low count)
#
##############################################################################################################################
plotTruth <- function(value, truth, maes, rmses, isGood, basename = basename) {
  value <- value[isGood]
  truth <- truth[isGood]
  maes  <- maes[isGood]
  rmses <- rmses[isGood]

  maes_sort  <- maes[order(maes)]
  rmses_sort <- rmses[order(maes)]
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"truth","pdf",sep="."))
  loginfo(paste("  Plotting output for",length(value),"classifiers."))
  loginfo(paste("    Saving all output plots to",pdfname))
  
  pdf(pdfname)
  setMargins(r = 1)
  
  cols <- head(getCols(), 3)
    
  classifiers <- names(value)
  for ( classifier in classifiers ) {
    truthOutput <- truth[[classifier]]
    scoreOutput <- value[[classifier]]
    mae         <- maes[[classifier]]
    rmse        <- rmses[[classifier]]
    good        <- isGood[[classifier]]
    
    ymin <- min(truthOutput, scoreOutput)
    ymax <- max(truthOutput, scoreOutput)
    
    ymin <- min(ymin / 1.05, ymin * 1.05)
    ymax <- max(ymax / 1.05, ymax * 1.05)
    
    title <- paste(classifier,"Response")
    info <- c()
    info <- paste(info, "  isGood:",as.character(good))
    info <- paste(info, "  MAE:",signif(mae,3)," (",loc(classifier, names(maes_sort)),") ")
    info <- paste(info, "  RMSE:",signif(rmse,3)," (",loc(classifier, names(rmses_sort)),") ")
    title <- paste(title, info, sep="\n")

    plot(truthOutput, type='b', lwd=2, lty=1, col=cols[1], log = '', pch=21, cex=1.75,
         ylim = c(ymin, ymax), axes=T, xlab="", ylab="Response", main=title)
    
    grid()
    par(new=T)
    plot(scoreOutput, type='b', lwd=2, lty=1, col=cols[2], log = '', pch=17, cex=1.5,
         ylim = c(ymin, ymax), axes=F, xlab="", ylab="", main="")
    
    par(new=T)
    res  <- scoreOutput / ( truthOutput + 1e-12 )
    res  <- ifelse(res > 2, 2, res)
    res  <- ifelse(res < 0, 0, res)
    mres <- max(0.5, max(abs(res-1)))
    plot(res, type='b', lwd=2, lty=3, col=cols[3], log = '', pch=19, cex=1.5,
         ylim = c(1 - mres, 1 + mres), axes=F, xlab="", ylab="", main="")
    axis(side = 4)
    mtext(side = 4, line = 3, 'Residual')    
    
    addLegend("topright", legend=c("Truth", "Score", "Redidual"), pch=c(21, 17, 19), col=cols, horiz=TRUE, bty='n', cex=0.8)    
    #legend("top", inset=.05, title="Classifier Response", c("Truth", "Score"), col = cols, fill = cols, horiz = F)
    setMargins()
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname))
}

plotTruth2D <- function(value, truth, maes, rmses, isGood, basename = basename) {
  require(MASS)
  library(RColorBrewer)
  require(hexbin)
  
  value <- value[isGood]
  truth <- truth[isGood]
  maes  <- maes[isGood]
  rmses <- rmses[isGood]

  maes_sort  <- maes[order(maes)]
  rmses_sort <- rmses[order(maes)]
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"truth2D","pdf",sep="."))
  loginfo(paste("  Plotting output for",length(value),"classifiers."))
  loginfo(paste("    Saving all output plots to",pdfname))
  
  pdf(pdfname)
  setMargins(r = 1)
  plot(NA, xlim = c(-500, 500), ylim = c(0, 100))
  
  cols <- head(getCols(), 3)
  rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r <- rf(32)
  
  classifiers <- names(value)
  for ( classifier in classifiers ) {
    truthOutput <- truth[[classifier]]
    scoreOutput <- value[[classifier]]
    mae         <- maes[[classifier]]
    rmse        <- rmses[[classifier]]
    good        <- isGood[[classifier]]

    xmin <- min(truthOutput)    
    xmax <- max(truthOutput)    
    ymin <- min(scoreOutput)
    ymax <- max(scoreOutput)
    
    xmin <- min(xmin / 1.15, xmin * 1.15)
    xmax <- max(xmax / 1.15, xmax * 1.15)
    ymin <- min(ymin / 1.15, ymin * 1.15)
    ymax <- max(ymax / 1.15, ymax * 1.15)
    
    ymax <- xmax
    ymin <- xmin

    fd      <- fitdistr(truthOutput - scoreOutput, "normal")
    fitvals <- fd[["estimate"]]
    fiterrs <- fd[["sd"]]
    loglk   <- fd[["loglik"]]
    n       <- fd[["n"]]
    chi2nof <- -loglk / n
    
    
    title <- paste(classifier,"Response")
    info <- c()
    info <- paste(info, "  isGood:",as.character(good))
    info <- paste(info, "  MAE:",signif(mae,3)," (",loc(classifier, names(maes_sort)),") ")
    info <- paste(info, "  RMSE:",signif(rmse,3)," (",loc(classifier, names(rmses_sort)),") ")
    #title <- expression(paste("fit:", mu, .(fitvals[["mean"]]), sigma, fitvals[["sd"]] ))
    #finfo <- paste(finfo, "  ",expression(mu),":",signif(fitvals[["mean"]],1),"(",signif(fiterrs[["mean"]],1),")")
    #finfo <- paste(finfo, "  ",expression(sigma),":",signif(fitvals[["sd"]],1),"(",signif(fiterrs[["sd"]],1),")")
    #fnfo <- paste(finfo, "  ",expression(chi),"^2:",signif(chi2nof,2))
    title <- paste(title, info, sep="\n")
    
    df <- data.frame("truth"=truthOutput, "prediction"=scoreOutput)
    if      ( max(abs(ymax - ymin), abs(xmax - xmin)) > 500 ) { bins <- 100 }
    else if ( max(abs(ymax - ymin), abs(xmax - xmin)) > 100 ) { bins <- 25 }
    else                               { bins <- 10 }
    h <- hexbinplot(prediction~truth, data=df, colramp=rf, mincnt=1, trans=log, inv=exp, 
                    xbins = bins, xlim = c(xmin, xmax), ybins = bins, ylim = c(ymin, ymax), main=title)
    #plot(h)
    #print(paste(xmin + 0.1*(xmax-xmin), ymax - 0.1*(ymax-ymin)))
    #text(xmin + 0.1*(xmax-xmin), ymax - 0.1*(ymax-ymin), labels = bquote(mu ~ "=" ~ .(fitvals[["mean"]]) ~ "\u00b1" ~ .(fitvals[["sd"]]) ), cex=1.2)
    #text(xmin, ymax, expression(f(x)== paste(frac(1, sqrt(2*pi* sigma^2 ) ), " ", e^{frac(-(x-mu)^2, 2*sigma^2)})), cex=1.2)
    
    #abline(coef = c(0,1), lty=5, col='black')
    
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname))
}



##############################################################################################################################
#
# Relative Difference (Regression only)
#
##############################################################################################################################
plotResidualHist <- function(counts, pts, cuts, maes, rmses, isGood, basename) {
  pdfname <- file.path(mvadir, "plots", paste(basename,"reshst","pdf",sep="."))
  loginfo(paste("  Plotting residuals for",length(pts),"classifiers."))
  loginfo(paste("    Saving all residuals plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  classifiers <- names(pts)
  for ( classifier in classifiers ) {
    #valRMSE <- rmse(diff)
    #valMAE  <- mae(diff)
    #mtxt    <- paste("RMSE =",signif(valRMSE,3),"    MAE =",signif(valMAE,3))
    #loginfo(paste(responseName,modelname,mtxt))
    
    pt       <- pts[[classifier]]
    count    <- counts[[classifier]]
    sigmacut <- cuts[[classifier]]
    mae      <- signif(maes[[classifier]], 3)
    rmse     <- signif(rmses[[classifier]], 3)
    good     <- isGood[[classifier]]
    
    maes_sort  <- maes[order(maes)]
    rmses_sort <- rmses[order(maes)]
    
    colors <- replicate(length(pt), "black")
    colors[pt >= sigmacut["-3sigma"] & pt <= sigmacut["3sigma"]] <- "red"
    colors[pt >= sigmacut["-2sigma"] & pt <= sigmacut["2sigma"]] <- "gold"
    colors[pt >= sigmacut["-1sigma"] & pt <= sigmacut["1sigma"]] <- "green"
    
    
    title <- paste(classifier,"Residuals")
    info <- c()
    info <- paste(info, "  isGood:",as.character(good))
    info <- paste(info, "  MAE:",signif(mae,3)," (",loc(classifier, names(maes_sort)),") ")
    info <- paste(info, "  RMSE:",signif(rmse,3)," (",loc(classifier, names(rmses_sort)),") ")
    title <- paste(title, info, sep="\n")
    
    plot(x = pt, y = count, type='p', pch=19, col = colors, cex=0.75, axes = F, las=2, xlab="MVA - Truth", ylab="Counts", main=title)
    axis(1, at = pretty(pt, n = 10), labels = pretty(pt, n = 10))
    axis(2, at = pretty(count, n = 10), labels = pretty(count, n = 10))
    
    
    box()
    grid(nx = 10)
    
    cols <- c("green", "gold", "red")    
    legend("topright", inset=.05, title="Residuals", c("+/- 1sigma", "+/- 2sigma", "+/- 3sigma"), col = cols, fill = cols, horiz = F)    
    
    if ( F ) {
      mtxt <- paste("MAE =",mae,"   RMSE =",rmse)
      mtext(mtxt, side = 3)
    }
    
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname))
  #  if ( nrow(results) == 0 ) { results <- data.frame(c(valRMSE, valMAE, valStDev)) }
  #  else { results <- cbind(results, c(valRMSE, valMAE, valStDev)) }
}


plotResiduals <- function(value, truth, normfits, maes, rmses, isGood, basename = basename) {
  pdfname <- file.path(mvadir, "plots", paste(basename,"resid","pdf",sep="."))
  loginfo(paste("  Plotting residuals for",length(value),"MVAs."))
  loginfo(paste("    Saving all residuals plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  maes_sort  <- maes[order(maes)]
  rmses_sort <- rmses[order(maes)]

  pdf(pdfname)
  setMargins(r = 1)
  
  cols <- head(getCols(), 3)
  
  classifiers <- names(value)
  for ( classifier in classifiers ) {
    truthOutput <- truth[[classifier]]
    scoreOutput <- value[[classifier]]
    residuals   <- truthOutput - scoreOutput
    mae         <- maes[[classifier]]
    rmse        <- rmses[[classifier]]
    good        <- isGood[[classifier]]
    normfit     <- normfits[[classifier]]

    residuals   <- capOutputByQuantile(residuals, 0.01)
    maxVal      <- round(max(abs(residuals))+0.1, 1)
    breaks      <- 100
    if      ( length(residuals) < 25 )  { breaks <- 5 }
    else if ( length(residuals) < 100 ) { breaks <- 20 }
    h <- hist(residuals, breaks = seq(-1*maxVal, maxVal, length.out = breaks), plot = F)
    
    fitvals <- normfit[["estimate"]]
    fiterrs <- normfit[["sd"]]
    mean  <- fitvals[["mean"]]
    sigma <- fitvals[["sd"]]
    meanerr  <- fiterrs[["mean"]]
    sigmaerr <- fiterrs[["sd"]]
    loglik   <- normfit[["loglik"]]
    n        <- normfit[["n"]]
    chi2nof  <- -loglik / n
    
    title <- paste(classifier,"Residuals")
    info <- c()
    info <- paste(info, "  isGood:",as.character(good))
    info <- paste(info, "  MAE:",signif(mae,3)," (",loc(classifier, names(maes_sort)),") ")
    info <- paste(info, "  RMSE:",signif(rmse,3)," (",loc(classifier, names(rmses_sort)),") ")
    title <- paste(title, info, sep="\n")


    x <- h[["mids"]]
    
    y <- h[["counts"]]
    tmp <- data.frame(y,x)
    tmp <- tmp[tmp$y > 0,]
    y <- tmp[,"y"]
    x <- tmp[,"x"]
    xmax <- max(max(x) / 1.01, max(x) * 1.01)
    xmax <- max(abs(min(x)), abs(xmax))
    xmin <- -1*xmax
    ymax <- max(y)*1.1
    ymin <- 0.9
    plot(y~x, type='b', lwd=2, lty=1, col=cols[1], log = 'y', pch=19, cex=1.25,
         ylim = c(ymin, ymax), xlim = c(xmin, xmax), axes=T, xlab="", ylab="", main=title)
    
    text(xmin + 0.1*(xmax-xmin), ymax - 0.1*(ymax-ymin), 
         labels = bquote(mu ~ "=" ~ .(signif(mean,1)) ~ "\u00b1" ~ .(signif(meanerr,1)) ), cex=1)
    text(xmin + 0.1*(xmax-xmin), ymax - 0.4*(ymax-ymin),
         labels = bquote(sigma ~ "=" ~ .(signif(sigma,2)) ~ "\u00b1" ~ .(signif(sigmaerr,2)) ), cex=1)
    text(xmin + 0.1*(xmax-xmin), ymax - 0.6*(ymax-ymin), 
         labels = bquote(chi^2 ~ "=" ~ .(signif(chi2nof,2)) ), cex=1)
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname))
}  








##############################################################################################################################
#
# MAE (Regression only)
#
##############################################################################################################################
plotMAE <- function(maes, ppp = 20, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"mae","pdf",sep="."))
  loginfo(paste("  Plotting MAE for",length(maes),"classifiers."))
  loginfo(paste("    Saving all MAE plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  items <- sortAndGroupItems(maes, ppp, decreasing = F)
  loginfo(paste("  Writing",length(items),"MAE pages."))
  
  setMargins(b = 2)  
  for ( item in items ) {
    vals <- maes[item]
    bp   <- barplot(vals, horiz=F, las=2, col='darkorange', ylim=c(0, 1.2*max(vals)), main="MVA Mean Absolute Errors (MAE)")    
    x    <- bp[,1]
    y    <- unname(vals)
    txt  <- signif(y, 2)
    ypos <- 1.075*y
    #ypos <- ifelse(y > 5, y + 0.6, y + 0.2)
    text(x = x, y = ypos, txt, srt=90, cex=0.75, col='dodgerblue')
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}


##############################################################################################################################
#
# RMSE (Regression only)
#
##############################################################################################################################
plotRMSE <- function(rmses, ppp = 20, basename, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"rmse","pdf",sep="."))
  loginfo(paste("  Plotting RMSE for",length(rmses),"classifiers."))
  loginfo(paste("    Saving all RMSE plots to",pdfname))
  
  pdf(pdfname)
  setMargins()
  
  items <- sortAndGroupItems(rmses, ppp, decreasing = F)
  loginfo(paste("  Writing",length(items),"RMSE pages."))
  
  setMargins(b = 2)  
  for ( item in items ) {
    vals <- rmses[item]
    bp   <- barplot(vals, horiz=F, las=2, col='darkorange', ylim=c(0, 1.2*max(vals)), main="MVA Root Mean Squared Errors (RMSE)")    
    x    <- bp[,1]
    y    <- unname(vals)
    txt  <- signif(y, 2)
    ypos <- 1.075*y
    #ypos <- ifelse(y > 5, y + 0.6, y + 0.2)
    text(x = x, y = ypos, txt, srt=90, cex=0.75, col='dodgerblue')
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}


##############################################################################################################################
#
# Gaussian Fits to Residuals
#
##############################################################################################################################
plotFitVals <- function(normfits, ppp = 20, basename) {
  pdfname <- file.path(mvadir, "plots", paste(basename,"fits","pdf",sep="."))
  loginfo(paste("  Plotting RMSE for",length(normfits),"classifiers."))
  loginfo(paste("    Saving all RMSE plots to",pdfname))
  
  pdf(pdfname)
  
  fitvals <- getListEntries(normfits, "estimate")
  fiterrs <- getListEntries(normfits, "sd")
  
  means  <- getVectorEntries(fitvals, "mean")
  sigmas <- getVectorEntries(fitvals, "sd")
  meanerrs  <- getVectorEntries(fiterrs, "mean")
  sigmaerrs <- getVectorEntries(fiterrs, "sd")
  
  items <- sortAndGroupItems(sigmas, ppp, decreasing = F)
  loginfo(paste("  Writing",length(items),"RMSE pages."))
  
  cols <- head(getCols(), 2)
  
  setMargins(l = 2, b=0.5)
  for ( item in items ) {
    par(mfrow=c(2,1))

    vals <- sigmas[item]
    plot(x = vals, y = seq(item), xlab="", ylab="", pch=19, col=cols[1], axes=F, main="Width of Gaussian Fit")
    axis(1, at = pretty(vals, n = 10))
    axis(2, at = seq(length(item)), labels = item, las=2, cex.axis=0.65)
    box()
    
    vals <- means[item]
    plot(x = vals, y = seq(item), xlab="", ylab="", pch=19, col=cols[2], axes=F, main="Mean of Gaussian Fit")
    axis(1, at = pretty(vals, n = 10))
    axis(2, at = seq(length(item)), labels = item, las=2, cex.axis=0.65)
    box()
  }
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname)) 
}




##########################################
#
# Feature Importance
#
##########################################
printVarImp <- function(predictions, pdfname) {
  require(caret)
  if ( length(grep("VarImp.pdf", pdfname)) == 0 ) {
    pdfname <- gsub(".pdf", "-VarImp.pdf", pdfname)
  }
  responseNames <- names(testingdata)
  models        <- names(predictions)
  
  pdf(pdfname)
  setMargins()
  for ( responseName in responseNames ) {
    for ( modelname in models ) {
      #print(paste(modelname, responseName))
      fit <- predictions[[modelname]][["fits"]][[responseName]][["prob"]]
    }
  }
  dev.off()
}


printModelResults <- function(predictions, testingdata, pdfname) {
  if ( length(grep("Output.pdf", pdfname)) == 0 ) {
    pdfname <- gsub(".pdf", "-Output.pdf", pdfname)
  }
  responseNames <- names(testingdata)
  models        <- names(predictions)
  print(pdfname)
  print(responseNames)
  print(models)
  
  pdf(pdfname)
  setMargins()
  for ( responseName in responseNames ) {
    truth <- testingdata[[responseName]][["Y"]]
    print(head(truth))
    lvls <- levels(truth)
    if ( length(lvls) != 2 ) {
      loginfo("Don't know what to the more than two levels for printModelResults")
      print(lvls)
      return(NULL)
    }
    pos <- lvls[which(lvls == responseName)]
    neg <- lvls[which(lvls != responseName)]
    levels(truth)[levels(truth)==pos] <- "Positive"
    levels(truth)[levels(truth)==neg] <- "Negative"
    print(head(truth))
    
    for ( modelname in models ) {
      score <- predictions[[modelname]][["pred"]][[responseName]][["prob"]]
      print(head(score))
      df <- data.frame(score, truth)
      g <- ggplot(df, aes(x=score, fill=truth)) + ggtitle(paste(modelname,"Output")) + geom_histogram(binwidth = 0.01)
      print(g)
      
      g <- ggplot(df, aes(x=score, fill=truth, y=..density..)) + ggtitle(paste(modelname,"Output")) + geom_histogram(binwidth = 0.01)
      print(g)
    }
  }
  dev.off()
  loginfo(paste("Wrote",pdfname))
}



dummy <- function() {
  if ( require(RColorBrewer) & drawProbs ) {
    pdfname <- "roc.pdf"
    probmodels <- names(performances)
    if ( T ) {
      for ( responseName in responseNames ) {
        vals <- list()
        for ( modelname in probmodels ) { vals[[modelname]] <- performances[[modelname]][[responseName]] }
        printROCs(vals, "multiROC.pdf", responseName)
      }
    }
    if ( F ) {
      for ( responseName in responseNames ) {
        
        mpred <- list()
        confM <- list()
        aucs  <- list()
        mperf <- list()
        for ( modelname in probmodels ) {
          mperf[[modelname]] <- performance(performances[[modelname]][[responseName]],"tpr","fpr")
          aucs[[modelname]] <- unlist(slot(performance(performances[[modelname]][[responseName]], measure = "auc"), "y.values"))
        }
        saucs <- aucs[order(unlist(aucs),decreasing=TRUE)]
        saucs <- unlist(saucs)
        
        dc <- unlist(aucs[order(unlist(aucs), decreasing = F)])
        rankedmodels <- names(dc)
        
        maxC <- 12
        maxM <- length(rankedmodels)
        vals <- rev(as.integer(seq(1, maxM, length.out = min(maxC, maxM))))
        cols=brewer.pal(maxC,"Set3")
        #vals <- c(1)
        legnames <- c()
        for ( i in 1:length(vals) ) {
          modelname <- rankedmodels[[vals[[i]]]]
          if ( i == 1 ) { plot(mperf[[modelname]], lty=1, lwd=2, col = cols[i], main=responseName) }
          else { plot(mperf[[modelname]], lty=1, lwd=2, col = cols[i], add=T) }
          
          if ( i == 1 ) { 
            par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
            grid()
          }
          auc <- aucs[[modelname]]
          legname <- paste(signif(auc,3), "-", modelname)
          legnames <- c(legnames, legname)
        }
        legend("bottomright", inset=.05, title="Model Performance",
               legnames, col = cols, fill = cols, horiz = F)
      }    
      dev.off()
      loginfo(paste("Wrote",pdfname))
    }
  }
}


getMultiROCs <- function(preds) {
  rocs <- list()
  for ( col in colnames(pred[["prob"]]) ) {
    probvals  <- pred[["prob"]][,col]
    classvals <- pred[["class"]] == col
    #rocs[col] <- performance(
    rocs[col] <- prediction(probvals, classvals) #,"tpr","fpr")
  }
  return( rocs )
}

getMultiTestResults <- function(preds, cmat) {
  rocs <- getMultiROCs(preds)
  tps <- sapply(rocs, function(x) x@tp)
  fps <- sapply(rocs, function(x) x@fp)
  cuts <- sapply(rocs, function(x) x@cutoffs)
  cols <- names(sapply(tps, function(x) x[0]))
  save(cmat, preds, rocs, file="tmp.rData")
  print(rocs)
  
  tpr <- list()
  fpr <- list()
  aucs <- list()
  for ( col in cols ) {
    tpr[[col]] <- tps[[col]] / max(tps[[col]])
    fpr[[col]] <- fps[[col]] / max(fps[[col]])
    aucs[[col]] <- getAUC(tpr[[col]], fpr[[col]])
  }
  print(aucs)
  print(cuts)
  f()
  
  tpdf <- as.data.frame(t(unlist(sapply(tps, function(x) x[1]))))
  fpdf <- as.data.frame(t(unlist(sapply(fps, function(x) x[1]))))
  for ( i in 2:length(tps[[1]]) ) {
    tpdf <- rbind(tpdf, t(unlist(sapply(tps, function(x) x[i]))))
  }
  for ( i in 2:length(fps[[1]]) ) {
    fpdf <- rbind(fpdf, t(unlist(sapply(fps, function(x) x[i]))))
  }
  
  save(tpdf, file="tmp.rData")
  
  print(tps)
  print(tpdf)
  maxitpr <- apply(tpdf, 1, function(x) max(max(x), 1))
  maxifpr <- apply(fpdf, 1, function(x) max(max(x), 1))
  print(maxitpr)
  print(maxifpr)
  itpr <- apply(tpdf, 2, function(x) x/maxitpr[x])
  ifpr <- apply(fpdf, 2, function(x) x/maxifpr[x])
  aucs <- list()
  for ( col in cols ) {
    print(paste(col,length(itpr[,col]), length(ifpr[,col])))
    print(itpr[,col])
    print(ifpr[,col])
    aucs[[col]] <- getAUC(itpr[,col], ifpr[,col])
  }
  print(aucs)
  f()
  tp <- apply(as.matrix(tpdf), 1, mean)
  fp <- apply(as.matrix(fpdf), 1, mean)
  tpr <- tp / max(tp)
  fpr <- fp / max(fp)  
  rAUC <- getAUC(tpr, fpr)
  
  return( list("tpr"=tpr, "fpr"=fpr, "auc"=rAUC))
}

getTestVals <- function(method, preds, testY, responseName) {
  probs <- pred[["prob"]]
  clss <- testY == responseName
  if ( method == "nnet" | method == "randomForest" | method == "lda" |
       method == "qda" | method == "bagging" | method == "avNNet") {
    prob <- probs[,responseName]
    return( list("prob"=prob, "class"=clss))
  } else if ( method == "rpart" ) {
    prob <- probs[,responseName]
    return( list("prob"=prob, "class"=clss))
  } else if ( method == "svm" ) {
    prob <- attr(probs, "probabilities")[,response]    
    return( list("prob"=prob, "class"=clss))
  } else if ( method == "knn" ) {
    prob <- attr(probs, "prob")    
    return( list("prob"=prob, "class"=clss))
  } else if ( method == "ctree" | method == "J48" | method == "gbm" | method == "C5.0" | method == "nb" ) {
    prob <- probs[,responseName]
    clss <- makeYfac(Y = testY, responseName = responseName)
    return( list("prob"=prob, "class"=clss))
  } else if ( method == "glm" | method == "glm-aic" | method == "glm-bic" ) {
    prob <- probs
    return( list("prob"=prob, "class"=clss))
  } else if ( method == "bartMachine" ) {
    prob <- probs
    return( list("prob"=prob, "class"=clss))
  }
  
  print(paste("No test value for", method))  
}

plotData <- function(trainingdata, testingdata, pdfname) {
  pdf(pdfname)
  for ( responsename in names(trainingdata) ) {
    y <- trainingdata[[responsename]][,1]
    x <- trainingdata[[responsename]][,-1]
    for ( var in colnames(x) ) {
      loginfo(paste("plot",responsename,"~",var))
      plot(y~x[,var], xlab=var, ylab=responsename, pch=19, col='dodgerblue')
    }
  }
  dev.off()
  loginfo(paste("Wrote", pdfname))
}
