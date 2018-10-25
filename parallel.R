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
# Parallel Functions
#
###########################################################################################
getCores <- function(maxFrac = 0.75, maxCores = 8) {
  if ( !exists("detectCores") ) { require(parallel) }
  
  nCores    <- detectCores()
  usedCores <- floor(nCores * maxFrac)
  if ( usedCores > maxCores ) { usedCores = maxCores }
  
  loginfo(paste("  ----> Using",usedCores,"of available",nCores))
  
  return( usedCores )
}

setupParallel <- function(maxFrac = 0.75, maxCores = 8) {
  useCaret       <- get0("useCaret")
  useMLR         <- get0("useMLR")
  useNative      <- get0("useNative")
  
  require(doParallel)  
  nCores  <- getCores(maxFrac, maxCores)
  
  if ( useCaret ) {
    if ( !exists("registerDoMC") ) { require(doMC) }
    loginfo(paste("  ----> Setup Caret to run with",nCores,"cores."))
    registerDoMC(cores = nCores)
  }
  
  if ( useMLR ) {
    if ( !exists("parallelStartSocket") ) { require(parallelMap) }
    loginfo(paste("  ----> Setup MLR to run with",nCores,"cores."))
    parallelStartMulticore(cpus = nCores)
  }
  
  return( nCores )
}

stopParallel <- function() {
  useCaret       <- get0("useCaret")
  useMLR         <- get0("useMLR")
  useNative      <- get0("useNative")

  if ( useMLR ) { parallelStop() }
}