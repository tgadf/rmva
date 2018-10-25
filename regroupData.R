mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

fname <- file.path(mvadir, "helper.R")
stopifnot(file.exists(fname))
source(fname)

##############################################################################################################################
#
# Check Column Variance
#
##############################################################################################################################
removeLowColVariance <- function(coldata, colname, cutoff = "binomial", removeFeatures = F, ...) {
  options <- unlist(list(...))
  debug   <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  tmpData <- sample(coldata, size = 100, replace = T)
  nUnique <- length(table(tmpData))
  
  if ( nUnique <= 10 ) {
    minLevel <- min(table(coldata))
    if ( minLevel < 10 ) { writeLines(paste("Likely low variance in",colname)) }
  }
}

