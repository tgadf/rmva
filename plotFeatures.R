mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))


isLogScale <- function(colData) {
  isLog <- F
  range <- quantile(colData, probs = c(0.05, 1-0.05))
  if ( min(colData) < 0 ) {
    if ( range["5%"] > 0 & range["95%"] > 0 ) {
      rat <- max(range["5%"] / range["95%"], range["95%"] / range["5%"])
      if ( rat > 100 ) { isLog <- T }
    }
  }
  return( isLog )
}


plotFeature <- function(feature, colData, targetData, positiveTarget) {
  loginfo(paste("Writing", feature), ind = 2)
  library(RColorBrewer)
  rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r <- rf(32)
  require(hexbin)
  
  corTarget <- cor(x = colData, y = targetData)
  
  samples <- sample(colData, 100, replace = T)
  nUnique <- length(unique(samples))
  maxBins <- min(nUnique, 50)
  
  if ( is.null(positiveTarget) ) {
    bins <- hist(colData, breaks = maxBins, plot = F)
    xValues <- bins[["mids"]]
    output  <- bins[["counts"]]
    title <- paste(feature,"Output")
    info  <- paste("Cor(target) =",round(corTarget,2))
    title <- paste(title, info, sep="\n")
    ymax <- max(output)
    #plot(output~xValues, type='b', lwd=2, lty=1, col = 'dodgerblue', log = '', pch=19,
    #     ylim = c(0, ymax*1.15), axes=T, xlab=feature, 
    #     ylab=paste("Counts",paste(maxBins,"Bins",collapse = " "), sep=" / "), main=title)

    capVal <- quantile(colData, probs = 0.99)
    cData  <- ifelse(colData > capVal, capVal, colData)
    capVal <- quantile(targetData, probs = 0.99)
    tData  <- ifelse(targetData > capVal, capVal, targetData)
    df <- data.frame("Feature"=colData, "Target"=tData)
    h  <- hexbinplot(Target~Feature, data=df, colramp=rf, mincnt=1, trans=log, inv=exp, 
                     xlim = c(min(cData)/1.1, max(cData)*1.1), ylim = c(min(tData)/1.1, max(tData)*1.1),
                     main = title)
    plot(h)
  } else {
    posPos <- which(targetData == positiveTarget)
    negPos <- which(targetData != positiveTarget)
    
    posColData <- colData[posPos]
    negColData <- colData[negPos]
    
    bins <- hist(colData, breaks = maxBins, plot=F)
    breaks <- bins[["breaks"]]
    
    posOutput <- hist(posColData, breaks = breaks, plot=F)
    negOutput <- hist(negColData, breaks = breaks, plot=F)
    
    kTestVal <- ks.test(posColData, negColData)

    ymax <- max(posColData, negColData)
    title <- paste(feature,"Output")
    info <- paste("KS(+/-):",round(kTestVal,2))
    title <- paste(title, info, sep="\n")
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
}



####################################################################################################
# Plots all features with useful stuff
####################################################################################################
plotFeatures <- function(inputdf, targetcol, positiveTarget, basename) {
  pos <- loc(targetcol, colnames(inputdf))
  if ( is.null(pos) ) {
    stop(paste("Could not find",targetcol,"in input df"))
  }
  targetData  <- inputdf[,pos]
  featureData <- inputdf[,-pos]
  
  pdfname <- file.path(mvadir, "plots", paste(basename,"features","pdf",sep="."))
  pdf(pdfname)
  setMargins()
  
  for ( feature in colnames(featureData) ) {
    colData <- featureData[,feature]
    plotFeature(feature, colData, targetData, positiveTarget)
  }
  
  dev.off()
  loginfo(paste("    Saved all plots to",pdfname))
}