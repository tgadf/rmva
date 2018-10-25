load(file = "good.rData")

sink("~/Documents/mva/caretParams.R")

writeLines("####################################################################################################################")
writeLines("#")
writeLines("# Testing Caret Classifier")
writeLines("#")
writeLines("####################################################################################################################")
writeLines("\n\n")
writeLines("getCaretParams <- function(classifier) {\n")
writeLines("  params <- list()\n")
for ( classifier in good ) {
  params <- carets[[classifier]][["parameters"]]
  classparams <- params[,"parameter"]
  paramtypes  <- params[,"class"]
  #print(classifier)
  #print(paste("  ",paste(classparams, collapse = ", ")))

  best <- fits[[classifier]][["bestTune"]]
  if ( is.null(best) ) { next }

  writeLines(paste("  if ( classifier == \"",classifier,"\" ) {", sep=""))
  for ( i in seq(classparams) ) {
    param <- classparams[[i]]
    ctype <- paramtypes[[i]]
    if ( ctype == "numeric" | ctype == "logical" ) {
      writeLines(paste("    params[[\"",param,"\"]] <- ", best[,param] , sep=""))
    } else if ( ctype == "character" ) {
      writeLines(paste("    params[[\"",param,"\"]] <- \"", best[,param], "\"", sep=""))
    } else {
      print(param)
      print(ctype)
      print(best[,param])
      f()
      writeLines(paste("    params[[\"",param,"\"]] <- ", best[,param] , sep=""))
    }
  }
  writeLines("  }\n")
}

writeLines("}")
sink()