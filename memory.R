mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}

##############################################################################################################################
#
# Memory Function
#
##############################################################################################################################
formatMemory <- function(fsize) {
  if ( fsize > 1e9 ) {
    fsize <- fsize / 1e9
    ext   <- "GB"
  } else if ( fsize > 1e6 ) {
    fsize <- fsize / 1e6
    ext   <- "MB"
  } else if ( fsize > 1e3 ) {
    fsize <- fsize / 1e3
    ext   <- "kB"
  } else {
    fsize <- fsize / 1e3
    ext   <- "kB"
  }
  fsize <- signif(fsize, 3)
  retval <- paste(fsize,ext)
  return( retval )
}

memUsed <- function() {
  if ( suppressWarnings(library("pryr", logical.return = T)) ) {
    loginfo("")
    loginfo(paste("==================> Memory Used:",formatMemory(mem_used())))
    loginfo("")
  } else {
    loginfo("")
    loginfo(paste("==================> Memory Used: N/A"))
    loginfo("")
  }
}



fileSize <- function(rfile) {
  ext = "Bytes"
  if ( file.exists(rfile) ) {
    fsize = file.size(rfile)
    if ( fsize > 1e9 ) {
      fsize <- fsize / 1e9
      ext   <- "GB"
    } else if ( fsize > 1e6 ) {
      fsize <- fsize / 1e6
      ext   <- "MB"
    } else if ( fsize > 1e3 ) {
      fsize <- fsize / 1e3
      ext   <- "kB"
    }
    fsize <- signif(fsize, 3)
    retval <- paste(fsize,ext)
    return( retval )
  } else {
    return( "?" )
  }
}



objectSize <- function(object) {
  ext = "Bytes"
  if ( !is.null(object) ) {
    osize = object.size(object)
    if ( osize > 1e9 ) {
      osize <- osize / 1e9
      ext   <- "GB"
    } else if ( osize > 1e6 ) {
      osize <- osize / 1e6
      ext   <- "MB"
    } else if ( osize > 1e3 ) {
      osize <- osize / 1e3
      ext   <- "kB"
    }
    osize <- signif(osize, 3)
    retval <- paste(osize,ext)
    return( retval )
  } else {
    return( "?" )
  }
}

mem <- function(object) { return( objectSize(object)) }

showMem <- function(object) {
  memdf <- sort( sapply(object,object.size), decreasing = T )
  memdf <- round(memdf[memdf > 1000]/1e6,3)
  memdf <- as.data.frame(memdf)
  colnames(memdf)[1] <- "Size (>1kB) [MB]"
  print(memdf)
}