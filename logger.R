#############################################################################################
# Comments
#############################################################################################

isFutile <- function() {
  return( suppressWarnings(library("futile.logger", logical.return = T)) )
}

makeIndent <- function(ind) {
  if ( ind > 0 ) { return( paste(replicate(ind, " "), collapse = "") ) }
  return( "" )
}

loginfo <- function(txt, ind = 0) {
  useFutile <- isFutile()
  if ( useFutile ) { flog.info(paste(makeIndent(ind),txt,sep = "")) }
  else             { writeLines(paste("INFO:",makeIndent,txt,sep = "")) }
}

logdebug <- function(txt, ind = 0) {
  useFutile <- isFutile()
  if ( useFutile ) { flog.debug(paste(makeIndent(ind),txt,sep = "")) }
  else             { writeLines(paste("DEBUG:",makeIndent,txt,sep = "")) }
}

logwarn <- function(txt, ind = 0) {
  useFutile <- isFutile()
  if ( useFutile ) { flog.warn(paste(makeIndent(ind),txt,sep = "")) }
  else             { writeLines(paste("WARN:",makeIndent,txt,sep = "")) }
}

logerror <- function(txt, ind = 0) {
  useFutile <- isFutile()
  if ( useFutile ) { flog.error(paste(makeIndent(ind),txt,sep = "")) }
  else             { writeLines(paste("ERROR:",makeIndent,txt,sep = "")) }
}

initLogger <- function(debug = F) {
  useFutile <- isFutile()
  if ( useFutile ) { 
    flog.threshold(INFO)
    if ( debug ) { flog.threshold(DEBUG) }
  }
}
