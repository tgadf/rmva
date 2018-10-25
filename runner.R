if ( dir.exists("/home/af02717") ) { setwd("/home/af02717")}
if ( dir.exists("/Users/tgadfort/Documents") ) { setwd("/Users/tgadfort/Documents")}

mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("format.R", "loadData.R", "train.R", "replaceNA.R", "splitData.R", 
           "logger.R", "parallel.R", "targetParams.R", "results.R", "memory.R", 
           "fileio.R", "dataOps.R", "plotFeatures.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}

initLogger(T)
assign("maxCores", 6, envir = .GlobalEnv)
assign("maxFrac", 0.75, envir = .GlobalEnv)
assign("useCaret", F, envir = .GlobalEnv)
assign("useMLR", T, envir = .GlobalEnv)
assign("useNative", F, envir = .GlobalEnv)
assign("paramLVL", 1, envir = .GlobalEnv)


#############################################################################################
# Create Data
#############################################################################################
setData <- function(datasetname = NULL) {
  if ( is.null(datasetname) ) { 
    datasetname = get0("dataset")
  }
  assign("dataset", datasetname, envir = .GlobalEnv)
  retvals <- loadSplitData(datasetname) # fileio
  assign("traindata", retvals[["traindata"]], envir = .GlobalEnv)
  assign("testdata",  retvals[["testdata"]], envir = .GlobalEnv)
  assign("truthdata", retvals[["truthdata"]], envir = .GlobalEnv)
  assign("targetcol", retvals[["targetcol"]], envir = .GlobalEnv)
  assign("positiveTarget", retvals[["positiveTarget"]], envir = .GlobalEnv)
  assign("problemType", getProblemType(truthdata))
}

createData <- function(dataset) {
  loginfo(paste("Creating dataset:",dataset))
  assign("dataset",   dataset, envir = .GlobalEnv)
  memUsed()
  if ( exists("traindata") ) { rm("traindata", envir = .GlobalEnv) }

  
  #############################################################################################
  # Get Data
  #############################################################################################
  retval          <- loadData(dataset)
  memUsed()
  targetcol       <- retval[["targetcol"]]
  positiveTarget  <- retval[["target"]]
  fdata           <- retval[["data"]]
  print(colnames(fdata))
  if ( is.null(loc(targetcol, colnames(fdata))) ) {
    stop(paste("Target column",targetcol,"does not exist in dataset!"))
  }
  targetdata      <- fdata[,targetcol]
  targetType      <- getTargetType(targetdata = targetdata, mva = NULL)

  
  #############################################################################################
  # Clean Data
  #############################################################################################
  fracTrain <- 0.8
  fdata     <- formatData(fdata = fdata, targetcol = targetcol, targetType = targetType, 
                          positiveTarget = positiveTarget, fracTrain = fracTrain)
  memUsed()
  if ( !is.null(targetcol) ) { targetcol <- "TARGET" }
  loginfo(paste("Formatted dataset and now splitting test/train with fraction:",fracTrain))
  if ( is.null(dataset) ) { stop("Dataset is NULL") }

  
  ##########################################################################################
  # Split Data section
  ##########################################################################################
  fdata        <- splitTestTrainData(df = fdata, fracTrain = fracTrain, dSample = F)
  traindata    <- getTrainData(fdata)
  testdata     <- getTestData(fdata)
  truthdata    <- getTestTarget(fdata)
  problemType  <- getProblemType(truthdata)
  rm(fdata)
  saveSplitData(dataset, traindata, testdata, truthdata, targetcol, positiveTarget)
  memUsed()
  setData(dataset)
}

trainMVAs <- function(mvas, dataset = NULL) {
  loginfo(paste("Training",length(mvas),"MVAs"))
  
  results        <- list()
  traindata      <- get0("traindata")
  loginfo(paste("  Training data has size:",getDimStr(traindata)))
  targetcol      <- get0("targetcol")
  positiveTarget <- get0("positiveTarget")
  njobs          <- setupParallel(get0("maxFrac"), get0("maxCores"))
  maxGrid        <- njobs
  results        <- trainMVA(mvas = mvas, traindata, targetcol, positiveTarget, maxGrid = maxGrid)
  stopParallel()
  if ( !is.null(dataset) ) { saveFits(dataset, getFits(results), getTimes(results)) }
  else { return( getFits(results) ) }
}

testMVAs <- function(fits = NULL, datasetname = NULL) {
  if ( is.null(fits) ) {
    if ( is.null(datasetname) ) { datasetname <- get("dataset") }
    fits <- loadFits(dataset = datasetname)
    print(names(fits))
  }
  if ( is.null(fits) ) { stop("Fits are null. Stopping") }
  if ( !(is.null(fits[["fits"]])) ) { fits <- fits[["fits"]] }
  if ( is.null(datasetname) ) { loginfo(paste("Testing",length(fits),"MVAs. Returning output on command line.")) }
  else                        { loginfo(paste("Testing",length(fits),"MVAs for dataset:",datasetname)) }
  
  results        <- list()
  truthdata      <- get0("truthdata")
  testdata       <- get0("testdata")
  targetcol      <- get0("targetcol")
  targetType     <- get0("targetType")
  positiveTarget <- get0("positiveTarget")
  njobs          <- setupParallel(get0("maxFrac"), get0("maxCores"))
  results        <- testMVA(fits, truthdata, testdata, targetcol, positiveTarget, targetType, njobs)
  stopParallel()
  if ( !is.null(datasetname) ) { saveTests(dataset = datasetname, tests = getTests(results), times = getTimes(results)) }
  else { return( getTests(results) ) }
}


perfMVAs <- function(testResults = NULL, save = F) {
  if ( is.null(testResults) ) {
    datasetname <- get("dataset")
    testResults <- loadTests(dataset = datasetname)
  }
  if ( !(is.null(testResults[["testResults"]])) ) { testResults <- testResults[["testResults"]] } 
  results        <- list()
  truthdata      <- get0("truthdata")
  positiveTarget <- get0("positiveTarget")
  results        <- performanceMVA(testResults, truthdata, positiveTarget)
  return( getPerfs(results) )
}
  
runMVAs <- function(mvas, stopAfterTrain=F, stopAfterTest=F) {
  
  loginfo("Training MVAs...")
  fits <- trainMVAs(mvas)
  loginfo("Training MVAs... Done")
  if ( stopAfterTrain ) { return( fits ) }
  
  loginfo("Testing MVAs...")
  testResults <- testMVAs(fits)
  loginfo("Testing MVAs... Done")
  if ( stopAfterTest ) { return( testResults ) }
  
  loginfo("Performance for MVAs...")
  perfs <- perfMVAs(testResults)
  loginfo("Performance for MVAs... Done")
  
  loginfo(paste("Returning the following MVA performances:", paste(names(perfs), collapse = ", ")))
  return( perfs )
}


  
##########################################################################################
# Run MVA Section
##########################################################################################
plotMVA <- function(perfs, basename) {
  traindata      <- get0("traindata")
  targetcol      <- get0("targetcol")
  positiveTarget <- get0("positiveTarget")
  plotFeatures(inputdf = traindata, targetcol = targetcol, positiveTarget = positiveTarget, basename = basename)
  
  plotResults(mvaPerformances = perfs, basename = basename)  
}