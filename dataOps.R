mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("memory.R", "logger.R")
for ( ifile in files ) {
  fname <- file.path(mvadir, ifile)
  stopifnot(file.exists(fname))
  source(fname)
}

##############################################################################################################################
# Unique Values
##############################################################################################################################
getUniqueValues <- function(coldata, sampleSize = 100) {
  logdebug(paste("  Getting number of unique values with",sampleSize,"/",length(coldata)))
  nUnique <- length(unique(sample(coldata, size = sampleSize, replace = T)))
  return( nUnique )
}

getCuts <- function(coldata, nBins = 20, isLog = F, sampleSize = 100) {
  logdebug(paste("  Getting cuts for data using",nBins))
  maxBins <- min(nBins, getUniqueValues(coldata, sampleSize))
  minVal <- min(coldata)
  maxVal <- max(coldata)
  if ( isLog ) {
    if ( minVal <= 0.0 ) { minVal <- min(coldata[coldata > 0]) }
    if ( minVal <= 0.0 ) { logwarn("Can not generate log data for this"); return( NULL) }
    minVal <- log(minVal)
    if ( maxVal <= 0.0 ) { maxVal <- max(coldata[coldata > 0]) }
    if ( maxVal <= 0.0 ) { logwarn("Can not generate log data for this"); return( NULL) }
    maxVal <- log(maxVal)
  }
  cuts <- seq(minVal, maxVal, length.out = nBins)
  return( cuts )
}



##############################################################################################################################
# Split Numeric Data
##############################################################################################################################
splitData <- function(coldata, cuts = NULL) {
  logdebug("  Spliting data")
  if ( is.null(cuts) ) { cuts <- getCuts(coldata) }
  splitVals <- split(coldata, f = cut2(coldata, cuts = cuts, onlycuts = T))
  return( splitVals )
}


##############################################################################################################################
# Split Numeric Data by fixed size
##############################################################################################################################
splitItems <- function(items, modval) {
  logdebug(paste("    Split items by",modval,"mods."))
  classIDs <- sapply(seq(items), function(x) ceiling(x/modval))
  retvals  <- list()
  for ( i in seq(unique(classIDs)) ) { retvals[[i]] <- items[classIDs == i] }
  return( retvals )
}

groupItems <- function(items, modval) {
  logdebug(paste("  Group items by",modval,"mods."))
  classIDs <- sapply(seq(items), function(x) ceiling(x/modval))
  names(classIDs) <- items
  retvals  <- list()
  for ( i in seq(unique(classIDs)) ) { retvals[[i]] <- names(classIDs)[classIDs == i] }
  return( retvals )
}

sortAndGroupItems <- function(values, modval, decreasing = T) {
  logdebug(paste("  Sort and group items by",modval,"mods."))
  maxValues          <- lapply(values, function(x) max(x))
  orderedClassifiers <- names(sort(unlist(maxValues), decreasing = decreasing))
  items              <- groupItems(orderedClassifiers, modval)
  return( items )  
}


##############################################################################################################################
# Position Information
##############################################################################################################################
findPosition <- function(classname, names) {
  if ( is.null(names) ) { return( NULL ) }
  if ( class(names) == "data.frame" ) { names <- colnames(names) }
  ypos = which(names %in% c(classname))
  if ( length(ypos) == 0 ) { warning(paste("No",classname,"in colnames of length",length(names))); return( NULL ) }
  return( ypos )
}

loc <- function(classname, colnames) { return( findPosition(classname, colnames)) }


##############################################################################################################################
# Vector Information
##############################################################################################################################
getHalf      <- function(vec) { return( head(vec, n = round(length(vec)/2, 0)) ) }
getLastHalf  <- function(vec) { return( tail(vec, n = round(length(vec)/2, 0)) ) }
getN         <- function(vec, N) { return( head(vec, n=N) ) }
getLastN     <- function(vec, N) { return( tail(vec, n=N) ) }
getLosPos    <- function(vec) { return( length(vec) ) }


##############################################################################################################################
# Function and Data.Frame Info
##############################################################################################################################
getDimStr <- function(df) {
  return( paste(dim(df), collapse = " x ") )
}

getEllipse <- function(eval) {
  retvals <- list(eval)
  return( unlist(retvals) )
}


##############################################################################################################################
# Vector Remove/Insert
##############################################################################################################################
rmElement <- function(element, elements) {
  logdebug(paste("  Remove element:",element))
  pos <- loc(element, elements)
  if ( !is.null(pos) ) { elements <- elements[-pos] }
  return( elements )
}

retNumeric <- function(value) {
  logdebug(paste("  Returning numeric value of",value))
  retval <- suppressWarnings(as.numeric(value))
  if ( is.na(retval) ) { return( value ) }
  return( retval )
}


##############################################################################################################################
# Set data frame character columns to numeric
##############################################################################################################################
setCharacterToNumeric <- function(df) {
  indx <- sapply(df, is.character)
  if ( sum(indx, na.rm = T) > 0 ) {
    logdebug(paste("  Converting",sum(indx, na.rm = T),"character columns to numeric."))
    df[indx] <- lapply(df[indx], function(x) as.numeric(x))
  }
  return( df )
}



##############################################################################################################################
# improved list of objects
##############################################################################################################################
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}