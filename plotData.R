mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


################################################################################################################
#
# Correlation Matrix
#
################################################################################################################
plotDataCorrelations <- function(corrMatrix, basename) {
  pdfname <- file.path(mvadir, "plots", paste(basename,"corr","pdf",sep="."))
  loginfo(paste("Plotting correlation matrix for",getDimStr(inputData),"features."))
  loginfo(paste("  Saving all plots to",pdfname,"..."))
  
  pdf(pdfname)
  setMargins(l = 2)
  
  corrplot(corrMatrix, method="number", "upper", order="AOE", tl.cex = 0.5)
  dev.off()
  
  loginfo(paste("  Saving all plots to",pdfname,"... Done."))
}

  
plotTargetCorrelations <- function(corrVector, ppp = 20, basename) {
  pdfname <- file.path(mvadir, "plots", paste(basename,"corrtar","pdf",sep="."))
  loginfo(paste("Plotting correlation with target for",length(corrVector),"features."))
  loginfo(paste("  Saving all plots to",pdfname,"..."))
  
  pdf(pdfname)
  setMargins()
  ovals <- order(abs(corrVector), decreasing = T)
  oCorrVector <- corrVector[ovals]
  names(oCorrVector) <- names(corrVector)[ovals]
  print(oCorrVector)
  items <- splitItems(oCorrVector, ppp)
  loginfo(paste("  Writing",length(items),"target correlation pages."))
  
  setMargins(l = 2.5, t = 1, b = 1)
  for ( item in items ) {
    corrs    <- item
    nplots   <- length(item)
    cols     <- rev(topo.colors(n = nplots))
    bp <- barplot(rev(corrs), axes = F, names.arg = NA, horiz = T, col=cols, xlim=c(-1,1))
    axis(2, at=bp[,1], labels = names(item), las=2)
    axis(1, at=seq(-1,1,0.25), labels = seq(-1,1,0.25), las=2)
    axis(3, at=seq(-1,1,0.25), labels = seq(-1,1,0.25), las=2)
    grid(nx = 8, ny = NA)
  }
  dev.off()
  
  loginfo(paste("  Saving all plots to",pdfname,"... Done."))
  
}


getTargetCorrelations <- function(inputData, inputTarget) {
  cordata <- cor(x = inputData, y = inputTarget)[,1]
  names(cordata) <- colnames(inputData)
  return( cordata )
}


plotCorrelations <- function(traindata, targetcol, basename) {
  require(corrplot)
  
  loginfo(paste("plotCorrelations(",getDimStr(traindata),targetcol,basename,")"))
  pos         <- loc(targetcol, traindata)
  inputData   <- traindata[,-pos]
  inputTarget <- traindata[, pos]

  loginfo(paste("Compute correlation matrix with",ncol(inputData),"variables."))
  cordata <- cor(inputData)
  plotDataCorrelations(corrMatrix = cordata, basename = basename)
  cordata <- getTargetCorrelations(inputData, inputTarget)
  plotTargetCorrelations(corrVector = cordata, basename = basename)
}


saveCorrelations <- function(traindata, targetcol, basename) {
  require(corrplot)
  
  loginfo(paste("plotCorrelations(",getDimStr(traindata),targetcol,basename,")"))
  pos         <- loc(targetcol, traindata)
  inputData   <- traindata[,-pos]
  inputTarget <- traindata[, pos]
  
  loginfo(paste("Compute correlation matrix with",ncol(inputData),"variables."))
  cordata <- cor(inputData)
}


################################################################################################################
#
# Input Features
#
################################################################################################################
plotFeatures <- function(traindata, targetcol, basename) {
  pdfname <- file.path(mvadir, "plots", paste(basename,"features","pdf",sep="."))
  loginfo(paste("Plotting correlation with target for",ncol(traindata)-1,"features."))
  loginfo(paste("  Saving all plots to",pdfname,"..."))
  
  pdf(pdfname)
  setMargins()
  
  pos         <- loc(targetcol, traindata)
  inputData   <- traindata[,-pos]
  inputTarget <- traindata[, pos]

  cordata <- round(getTargetCorrelations(inputData, inputTarget), 2)
  for ( feature in colnames(inputData) ) {
    loginfo(feature)
    corval    <- cordata[feature]
    coldata   <- traindata[,feature]
    breaks    <- getCuts(coldata)
    tmp       <- hist(coldata, breaks = breaks, plot = F)
    counts    <- tmp[["density"]]
    xValues   <- tmp[["mids"]]
    print("Hist Vals")
    print(counts)
    print(xValues)
    
    title <- paste(feature, corval, sep="\n")
    ymax <- max(counts)
    barplot(height = counts, names.arg = xValues, ylim = c(0, ymax*1.25), axes=T, xlab="Feature Value", ylab="Density", main=title, las=2)
    writeLines("\n\n")
    if ( F ) {  
      grid()
      par(new=T)
      plot(negOutput~xValues, type='b', lwd=2, lty=1, col = 'darkorange', log = '', pch=17,
           ylim = c(0, ymax*1.25), axes=F, xlab="", ylab="", main="")
      
      cols <- c('dodgerblue', 'darkorange')
      legend("top", inset=.05, title="Classifier Output",
             c("Positive", "Negative"), col = cols, fill = cols, horiz = F)      
    }
  }
  
  dev.off()
  
  loginfo(paste("  Saving all plots to",pdfname,"... Done."))
  
  
  
  
}