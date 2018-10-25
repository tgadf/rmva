if ( dir.exists("/home/af02717") ) { setwd("/home/af02717")}
if ( dir.exists("/Users/tgadfort/Documents") ) { setwd("/Users/tgadfort/Documents")}

source("mva/helper.R")
  


getCaretGrid <- function(carets) {
  caretgrid <- list()
  pos <- loc("TARGET", colnames(traindata))
  y <- traindata[,pos]
  x <- traindata[,-pos]
  for ( classifier in names(carets) ) {
    info <- carets[[classifier]]
    result = tryCatch({
      gridval <- info$grid(x = x, y = y, len=3)
    }, error = function(e) {
      print(paste("MY_ERROR:  ",e))
      gridval <- NULL
    })
    if ( is.null(gridval) ) { next }
    print(gridval)
    caretgrid[[classifier]] <- gridval
  }
  
  save(caretgrid, file="caretgrid3.rData", compress = T)
}


checkTunes <- function(tunes) {
  vars <- list()

  for ( ifile in names(tunes) ) {
    bestTune <- tunes[[ifile]]
    for ( col in colnames(bestTune) ) {
      vars[[col]] <- c(vars[[col]], bestTune[,col])
    }
  }
  classtune <- data.frame("default"=character(),"minval"=character(),"maxval"=character(),"fixed"=logical())
  classtune <- data.frame()
    
  for ( var in names(vars) ) {
    freq <- sort(table(vars[[var]]), decreasing = T)
    print(paste("  Var:",var))
    print(freq)
    default <- names(freq[1])
    if ( length(freq) >= 3 ) {
      minval <- names(freq[2])
      maxval <- names(freq[3])
      fixed  <- F
    } else {
      minval <- 0
      maxval <- 0
      fixed  <- T
    }
    
    default <- retNumeric(default)
    minval <- retNumeric(minval)
    maxval <- retNumeric(maxval)
    if ( ncol(classtune) == 0 ) {
      classtune <- data.frame(c(default, minval, maxval, fixed))
      rownames(classtune) <- c("default", "minval", "maxval", "fixed")
    } else {
      classtune <- cbind(classtune, (c(default, minval, maxval, fixed)))
    }
    colnames(classtune)[ncol(classtune)] <- var
  }
  classtune <- t(classtune)

  return( classtune )  
}



saveTunes <- function(MVAtunes) {
  load("caretgrid1.rData")
  caretgrid1 <- caretgrid
  load("caretgrid3.rData")
  caretgrid3 <- caretgrid
  tunes <- list()
  sink("class.txt")
  carettunes <- list()
  for ( classifier in names(MVAtunes) ) {
    print(paste("========>",classifier))
    tunes[[classifier]] <- checkTunes(MVAtunes[[classifier]])
    #print(MVAtunes[[classifier]])
    print("  Best Tune (1)")
    print(tunes[[classifier]])
    print("  Caret Grid (1)")
    print(caretgrid1[[classifier]])
    print("  Caret Grid (3)")
    print(caretgrid3[[classifier]])
    writeLines("\n\n")
  }
  sink()
  
  save("caretparams"=tunes, file = "caretparams.rData")
}
  

checkAcc <- function(results) {
  minAcc <- 1
  maxAcc <- 0
  
  for ( ifile in names(results) ) {
    acc <- results[[ifile]][,"Accuracy"]
    minAcc <- min(minAcc, acc)
    maxAcc <- max(maxAcc, acc)
  }
}

load(file = file.path(mvadir, "caret", "good.rData"))

files <- Sys.glob(paths = "mva/caret/fits_*.rData")
MVAresults <- list()
MVAtunes   <- list()
for ( ifile in files ) {
  writeLines(ifile)
  lvals <- load(ifile)
  for ( classifier in names(results) ) {
    if ( is.null(MVAresults[[classifier]]) ) { MVAresults[[classifier]] <- list() }
    if ( is.null(MVAtunes[[classifier]]) )   { MVAtunes[[classifier]] <- list() }
    MVAresults[[classifier]][[ifile]] <- results[[classifier]]
    MVAtunes[[classifier]][[ifile]]   <- bestTune[[classifier]]
  }  
}