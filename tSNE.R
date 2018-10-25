mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


###########################################################################################
#
# MVA Performance
#
###########################################################################################
computeTSNE <- function(traindata, targetcol, positiveTarget) {
  require(Rtsne)
  
  loginfo("computeTSNE()")
    
  loginfo("Removing duplicates before running TSNE")
  pos <- loc(targetcol, colnames(traindata))
  tsnedata   <- traindata[,-pos]
  targetdata <- traindata[,pos]
  duplrows   <- which(duplicated(tsnedata))
  loginfo(paste("Found",length(duplrows),"duplicate rows. Removing them."))
  if ( length(duplrows) > 0 ) {
    tsnedata   <- tsnedata[-duplrows,]
    targetdata <- targetdata[-duplrows]
  }
  
  loginfo(paste("Computing TSNE for",paste(dim(tsnedata), collapse = " x "),"dataset..."))
  tsne_out <- Rtsne(as.matrix(tsnedata)) # Run TSNE
  loginfo(paste("Computing TSNE for",paste(dim(tsnedata), collapse = " x "),"dataset... Done"))
  return( list("tsne"=tsne_out, "target"=targetdata) )
}

plotTSNE <- function(tsneresults, positiveTarget) {
  require(Rtsne)

  tsne_out   <- tsneresults[["tsne"]]
  targetdata <- tsneresults[["target"]]
  ncol       <- tsne_out[["origD"]]
  nrow       <- tsne_out[["N"]]
  tsnedata   <- tsne_out[["Y"]]
  title      <- paste("tSNE Results for",nrow,"x",ncol,"dataset.")

  cols       <- replicate(length(targetdata), "black")
  if ( !(is.null(positiveTarget)) ) {
    pos        <- which(targetdata == positiveTarget)
    cols[pos]  <- "dodgerblue"
    pos        <- which(targetdata != positiveTarget)
    cols[pos]  <- "darkorange"
  } else {
    cuts <- cut(targetData, breaks = quantile(targetData, probs = c(0.0, 0.05, 0.34, 0.68, 0.95, 1.0)))
    for ( i in 1:nlevels(cuts) ) {
      pos        <- which(cuts == levels(cuts)[1])
      cols[pos]  <- "#d33f49"
      pos        <- which(cuts == levels(cuts)[2])
      cols[pos]  <- "#d7c0d0"
      pos        <- which(cuts == levels(cuts)[3])
      cols[pos]  <- "#eff0d1"
      pos        <- which(cuts == levels(cuts)[4])
      cols[pos]  <- "#77ba99"
      pos        <- which(cuts == levels(cuts)[5])
      cols[pos]  <- "#262730"
    }
  }
  
  plot(tsnedata, col=cols, xlab="tSNE x-axis", ylab="tSNE y-axis", main=title, pch=19, cex=0.25)
  if ( !(is.null(positiveTarget)) ) {
    cols <- c("dodgerblue", "darkorange")
    legend("topright", inset=.01, c("Pos", "Neg"), col = cols, fill = cols, horiz = F, cex=0.75)
  } else {
    cols <- c("#d33f49", "#d7c0d0", "#eff0d1", "#77ba99", "#262730")
    legend("topright", inset=.01, c("-2sig", "-1sig", "Cent", "1sig", "2sig"), col = cols, fill = cols, horiz = F, cex=0.75)
  }
  grid()
}