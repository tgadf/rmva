mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("memory.R", "dataOps.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}


##############################################################################################################################
# Find Files
##############################################################################################################################
findFiles <- function(dtype = NULL, prefix = NULL) {
  files <- NULL
  if ( is.null(dtype) & is.null(prefix) )   { files <- Sys.glob(file.path(mvadir, "output", "*.rData")) }
  if ( !is.null(dtype) & is.null(prefix) )  { files <- Sys.glob(file.path(mvadir, "output", paste(dtype,".","*.rData",sep=""))) }
  if ( is.null(dtype) & !is.null(prefix) )  { files <- Sys.glob(file.path(mvadir, "output", paste("*",prefix,".","*.rData",sep=""))) }
  if ( !is.null(dtype) & !is.null(prefix) ) { files <- Sys.glob(file.path(mvadir, "output", paste(dtype,".",prefix,".","*.rData",sep=""))) }
  if ( length(files) == 0 ) { return( NULL ) }
  return( files )
}


##############################################################################################################################
# Get Most Recent Files
##############################################################################################################################
getRecentFile <- function(dtype = NULL, prefix = NULL) {
  logdebug("Getting most recent file.")
  
  files <- findFiles(dtype, prefix)
  if ( is.null(files) ) { warning(paste("No files were found with type",dtype,"and prefix",prefix)); return( NULL ) }
  times <- file.mtime(files)
  df    <- data.frame("files"=files, "times"=times)
  df    <- df[rev(order(df$times)),]
  mostRecent <- as.character(df[1, "files"])
  return( mostRecent )
}


##############################################################################################################################
# Load Items From File
##############################################################################################################################
loadFile <- function(filename) {
  loginfo(paste("Loading file:",filename))
  
  if ( file.exists(filename) ) {
    logdebug(paste("Loading",filename,"..."))
    lvals <- load(filename)
    logdebug(paste("Loading",filename,"... Done"))
    logdebug(paste("Found the following items:",paste(lvals, collapse = ", ")))
    retvals <- lapply(lvals, function(x) get0(x))
    names(retvals) <- lvals
    return( retvals )
  } else {
    stop(paste("The file",filename,"does not exist."))
  }
}


##############################################################################################################################
# Load Test/Train/Truth Data
##############################################################################################################################
saveSplitData <- function(dataset = NULL, traindata = NULL, testdata = NULL, truthdata = NULL, 
                          targetcol = NULL, positiveTarget = NULL) {
  if ( is.null(dataset) ) { dataset <- get0("dataset") }
  loginfo(paste("Saving split dataset with name:",dataset))
  
  savename <- file.path(mvadir, "output", paste("data", dataset, make.names(Sys.time()), "rData", sep = "."))
  loginfo(paste("Saving train/test/truth data to", savename,"..."))
  save(traindata, testdata, truthdata, targetcol, positiveTarget, file = savename, compress = T)
  loginfo(paste("Saving train/test/truth data to", savename,"... Done"))
  loginfo(paste("File has size:",formatMemory(file.size(savename))))
}

loadSplitData <- function(dataset, forceReLoad = F) {
  loginfo(paste("Loading Split Dataset with name:",dataset))
  
  lvals <- c("traindata", "testdata", "truthdata", "targetcol", "positiveTarget")
  if ( forceReLoad | !all(sapply(lvals, function(x) exists(x))) ) {
    filename <- getRecentFile("data", dataset)
    if ( is.null(filename) ) { stop(paste("There is no split data with name:",dataset)) }
    retvals <- loadFile(filename)
  } else {
    loginfo(paste(paste(lvals, collapse = ", "),"all exist. Returning them."))
    retvals <- sapply(lvals, function(x) get(x))
  }
  showMem(retvals)
  return( retvals )
}


##############################################################################################################################
# Load Fits
##############################################################################################################################
saveFits <- function(dataset = NULL, fits = NULL, times = NULL) {
  if ( is.null(dataset) ) { dataset <- get0("dataset") }
  loginfo("Saving fits for dataset:",dataset)
  
  savename <- file.path(mvadir, "output", paste("fits", dataset, make.names(Sys.time()), "rData", sep = "."))
  showMem(fits)
  loginfo(paste("Saving fits/time to", savename,"..."))
  save(fits, times, file = savename, compress = T)
  loginfo(paste("Saving fits/time to", savename,"... Done"))
  loginfo(paste("File has size:",formatMemory(file.size(savename))))
}

loadFits <- function(dataset, forceReLoad = T) {
  loginfo(paste("Loading Fits for dataset:",dataset))
  
  lvals <- c("fits", "times")
  if ( forceReLoad | !all(sapply(lvals, function(x) exists(x))) ) {
    filename <- getRecentFile("fits", dataset)
    retvals <- loadFile(filename)
  } else {
    loginfo(paste(paste(lvals, collapse = ", "),"all exist. Returning them."))
    retvals <- sapply(lvals, function(x) get(x))
  }
  showMem(retvals)
  return( retvals )
}


##############################################################################################################################
# Load Tests
##############################################################################################################################
saveTests <- function(dataset = NULL, tests = NULL, times = NULL) {
  if ( is.null(dataset) ) { dataset <- get0("dataset") }
  loginfo("Saving tests for dataset:",dataset)
  
  savename <- file.path(mvadir, "output", paste("tests", dataset, make.names(Sys.time()), "rData", sep = "."))
  showMem(tests)
  loginfo(paste("Saving tests/time to", savename,"..."))
  save(tests, times, file = savename, compress = T)
  loginfo(paste("Saving tests/time to", savename,"... Done"))
  loginfo(paste("File has size:",formatMemory(file.size(savename))))
}

loadTests <- function(dataset, forceReLoad = T) {
  loginfo(paste("Loading Tests for dataset:",dataset))
  
  lvals <- c("tests", "times")
  if ( forceReLoad | !all(sapply(lvals, function(x) exists(x))) ) {
    filename <- getRecentFile("tests", dataset)
    retvals <- loadFile(filename)
  } else {
    loginfo(paste(paste(lvals, collapse = ", "),"all exist. Returning them."))
    retvals <- sapply(lvals, function(x) get(x))
  }
  if ( is.null(retvals[["testResults"]]) ) {
    retvals[["testResults"]] <- retvals[["tests"]]
    retvals[["tests"]] <- NULL
  }
  showMem(retvals)
  return( retvals )
}


##############################################################################################################################
# Sample dataset
##############################################################################################################################
sampleData <- function(fdata, sFraction) {
  loginfo(paste("Sampling data of size:",getDimStr(fdata),"with fraction:",sFraction))

  keep <- sample(nrow(fdata), size = as.integer(sFraction*nrow(fdata)), replace = T)
  retdata <- fdata[keep,]

  loginfo(paste("  New data size is:",getDimStr(retdata)))

  return( retdata )
}




############################################################################################################
# Get Sep Length (if any)
############################################################################################################
getSepLength <- function(line, sep, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  vals    <- gregexpr(pattern = sep, text = line)[[1]]
  nSep <- NULL
  if ( length(vals) > 1 ) {
    nSep <- 1
    for ( i in seq(vals) ) {
      diff <- vals[i+1] - vals[i]
      if ( diff > 1 ) { break }
      else            { nSep = nSep + 1 }
    }
  }
  return( nSep )
}


############################################################################################################
# Get Sep Lengths
############################################################################################################
getSepLengths <- function(line, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  seps <- c(",", " ", "\t", ";", ":")
  for ( sep in seps ) {
    sepL <- getSepLength(line = line, sep = sep, options)
    if ( !is.null(sepL) ) { 
      sepVal <- paste(rep(sep, length.out=sepL), collapse = "")
      return( sepVal )
      break
    }
  }
  return( NULL )
}


############################################################################################################
# Split Line by Sep
############################################################################################################
splitLineBySep <- function(line, sep, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  if ( !is.null(sep) ) {
    strVals    <- strsplit(x = line, split = sep)[[1]]
    return( strVals )
  } else {
    stop(paste("No separator in line:",line))
  }
}

splitLine <- function(line, ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  sep        <- getSepLengths(line, options)
  return( splitDataBySep(line, sep, options) )
}
