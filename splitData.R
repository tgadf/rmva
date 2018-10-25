mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "convData.R", "targetParams.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
#
# Down Sample (if needed)
#
##############################################################################################################################
downSampleData <- function(fdata, targetcol) {
  loginfo("  Downsampling data.")
  
  pos    <- loc(targetcol, colnames(fdata))
  if (is.null(pos)) { stop(paste("Target column",targetcol,"is not in final training dataset...")) }
    
  if ( ncol(fdata) > 2 ) {
    Xdata <- fdata[,-pos]
    Ydata <- fdata[,pos]
  } else {
    colname <- rmElement(targetcol, colnames(fdata))
    Xdata <- data.frame(fdata[,-pos])
    colnames(Xdata) <- colname
    Ydata <- fdata[,pos]
  }
  if ( class(Ydata) == "factor" ) {
    if ( debug ) { writeLines("  Y data is not a factor so temporarily setting it to factor in downSampleData()") }
  
    origClass <- class(Ydata)
    Ydata <- as.factor(Ydata)
    fdata  <- downSample(x = Xdata, y = Ydata, list = F)
    pos    <- loc("Class", colnames(fdata))
    colnames(fdata)[pos] <- targetcol
  }
  
  return( fdata )
}



##############################################################################################################################
#
# Create Testing and Training Datasets
#
##############################################################################################################################
splitTestTrainData <- function(df, fracTrain = 0.5, dSample = F) {
  require(caret)
  
  if ( class(df) == "data.frame" ) {
    logdebug("  Input is a data.frame")
    targetcol <- "TARGET"
  } else if ( class(df) == "list" ) {
    logdebug("   Input is a list") 
    targetcol <- df[["targetcolname"]]
    df <- df[["data"]]
  }

  if ( is.null(loc(targetcol, colnames = colnames(df))) ) {
    problemType <- getProblemType(NULL)
    loginfo(paste("  Creating training and testing data sets."))
  } else {
    problemType <- getProblemType(df[,targetcol])
    loginfo(paste("  Creating training and testing data sets for target:",targetcol))
  }
  
  
  if ( fracTrain >= 1 | fracTrain < 0 ) {
    stop(paste("Training fraction",fracTrain,"is either above 1 or below 0"))
  }
  
  
  # Check to see if there is already a train/test column set
  if ( is.null(loc("isTrain", colnames = colnames(df))) ) {
    loginfo("  Randomly creating train/test dataset")
    ind        <- sample(2, nrow(df), replace=TRUE, prob=c(fracTrain, 1-fracTrain))
    traindata  <- df[ind==1,]
    testdata   <- df[ind==2,]
  }
  else {
    loginfo("  Using predefined train/test dataset")
    traindata  <- df[df[,"isTrain"] == 1,]
    testdata   <- df[df[,"isTrain"] != 1,]
  }
  
  traindata[,"isTrain"] <- T
  testdata[,"isTrain"]  <- F
  df <- rbind(traindata, testdata)
  pos1 <- loc(targetcol, colnames(df))
  pos2 <- loc("isTrain", colnames(df))
  if ( problemType == "binaryClassification" ) {
    freqdf <- table(df[,c(pos1,pos2)])
  } else if ( problemType == "linearRegression" ) {
    loginfo("Summary of Training and Testing target datasets.")
    print(summary(traindata[,targetcol]))
    print(summary(testdata[,targetcol]))
  }
  
  if ( problemType == "binaryClassification" & dSample == T ) {
    logdebug("  Downsampling data (if needed).")
    traindata <- downSampleData(fdata = traindata, targetcol = targetcol, options)
    testdata  <- downSampleData(fdata = testdata,  targetcol = targetcol, options)
    df <- rbind(traindata, testdata)
  }
  
  if ( problemType == "binaryClassification" ) {
    writeLines("  Split train/test sample")
    if ( dSample ) {
      writeLines("    Pre Down Sample")
      print(freqdf)
    }
    pos1 <- loc(targetcol, colnames(df))
    pos2 <- loc("isTrain", colnames(df))
    freqdf <- table(df[,c(pos1,pos2)])
    if ( dSample ) {
      writeLines("    Post Down Sample")
    }
    print(freqdf)
    writeLines("")
  }
  
  return( df )
}