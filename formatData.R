checkColl <- function(tdata) {
  isCol <- T
  cols <- list()
  
  while ( isCol == T ) {
    isCol <- F
    cmat <- cor(tdata)
    diag(cmat) <- 0
    names <- colnames(cmat)
    for ( i in seq(ncol(cmat)) ) {
      pos <- which(cmat[,i] > 0.95)
      if ( length(pos) > 0 ) {
        cols[[names[i]]] <- pos
        tdata <- tdata[,-c(pos)]
        isCol <- T
        break
      }
    }
    if ( isCol ) { print(cols) }
  }
  
  return( cols )  
}

removeCollinearFeatures <- function(trainX, testX) {
  collinears <- checkColl(trainX)
  for ( coll in collinears ) { trainX <- trainX[,-coll] }
  for ( coll in collinears ) { testX <- testX[,-coll] }
  return( list("trainX"=trainX, "testX"=testX) )
}

makeYfacLevels <- function(Y, posClassName) {
  levels(Y) <- c(paste("Not", posClassName, sep = ""), posClassName)
  #Y <- relevel(Y, responseName)
  return( Y )
}

makeYfac <- function(Y, responseName) {
  Y <- as.factor(Y == responseName)
  Y <- makeYfacLevels(Y, responseName)
  return( Y )
}

renameLevel <- function(Y, posClass, newPosClass = NULL) {
  if ( is.null(newPosClass) ) { newPosClass <- posClass }
  newLevels <- c(paste("Not", newPosClass, sep = ""), newPosClass)
  levels(Y) <- newLevels
  return( Y )
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


findPosition <- function(classname, colnames) {
  if ( is.null(classname) ) { return( NULL ) }
  ypos = which(colnames %in% c(classname))
  if ( length(ypos) == 0 ) {
    writeLines(paste("Could not find",classname,"in [",colnames,"]"))
    return( NULL )
  }
  return( ypos )
}


checkFeatures <- function(data, cutoff = 0.05, removeFeatures = F) {
  lowvars <- c()
  data   <- removeNAs(data)
  for ( col in colnames(data) ) {
    cdata <- data[,col]
    if ( class(cdata) == "integer" | class(cdata) == "numeric" ) {
      if ( var(cdata, na.rm = T) < cutoff ) {
        writeLines(paste(col,"has low variance",sum(cdata)/length(cdata)))
        lowvars <- c(lowvars, col)
      }
    }
  }
  pos <- findPosition(lowvars, colnames(data))
  if ( removeFeatures ) {
    if ( !(is.null(pos)) ) { data   <- data[,-pos] }
    return( data )
  } else {
    return( pos )
  }
}


removeNAs <- function(data) {
  NAs <- apply(data, MARGIN = 2, function(x) sum(is.na(x)))
  if ( length(NAs) > 0 ) {
    NAs <- NAs[NAs > 0]
    for ( col in names(NAs) ) {
      writeLines(paste("Dropping",col,"due to",NAs[col],"NAs"))
      data[,col] <- NULL
    }
  }
  return( data )
}


convertFactors <- function(data) {
  cols <- colnames(data)
  pdim <- paste(dim(data), collapse = " x ")
  for ( col in cols ) {
    if ( class(data[,col]) == "character" | class(data[,col]) == "factor" ) { 
      if ( length(unique(data[,col])) == 2 ) {
        uvals <- unique(data[,col])
        pos <- findPosition("Y", uvals)
        if ( !(is.null(pos)) ) {
          newcol <- ifelse(coldata == "Y", 1, 0)
          data[,col] <- newcol
          writeLines(paste("Setting",col,"to binary 1,0 numeric output"))
        }
      }
    }
      
    if ( class(data[,col]) == "character" ) { data[,col] <- as.factor(data[,col]) }
    if ( class(data[,col]) == "factor" ) {
      coldata <- data[,col]
      uvals <- levels(coldata)
      newcolnames <- c()
      for ( uval in uvals ) {
        if ( uval == "?") { newcolname <- make.names(paste(col,"UNK",sep="")) }
        else { newcolname <- make.names(paste(col,uval,sep="")) }
        newcol <- ifelse(coldata == uval, 1, 0)
        data <- cbind(newcol, data)
        colnames(data)[1] <- newcolname
        newcolnames <- c(newcolnames, newcolname)
      }
      writeLines(paste("Factor: ",col,'    ->   ',paste(newcolnames, collapse = ", ")))
      data[,col] <- NULL
    }
  }
  ndim <- paste(dim(data), collapse = " x ")
  writeLines(paste("Previous Dim: ",pdim))
  writeLines(paste(" Convert Dim: ",ndim))
  return( data )
}


downSamp <- function(data, responseName, newResponseName = NULL) {
  colnames <- colnames(data)
  ypos     <- findPosition(responseName, colnames)
  dataX    <- data[,-ypos]
  dataY    <- as.factor(data[, ypos])
  if ( is.null(newResponseName) ) { newResponseName <- responseName }
  data   <- downSample(dataX, dataY, yname = newResponseName)
  return( data )
}


combineSignalAndBackground <- function(signaldata, backgrounddata, responseName="Class") {
  classval <- as.factor(c("Signal", "Background"))
  signaldata <- cbind("tmp"=replicate(nrow(signaldata), classval[1]), signaldata)
  backgrounddata  <- cbind("tmp"=replicate(nrow(backgrounddata), classval[2]), backgrounddata)
  data <- rbind(signaldata, backgrounddata)
  colnames(data)[1] <- responseName
  return( data )
}


createTestAndTrain <- function(data, className, trainFrac) {
  ypos <- findPosition(className, colnames(data))
  ind <- sample(2, nrow(data), replace=TRUE, prob=c(trainFrac, 1-trainFrac))
  train  <- data[ind==1,]
  test   <- data[ind==2,]
  trainX <- train[,-ypos]
  trainY <- train[, ypos]
  testX  <- test[,-ypos]
  testY  <- test[, ypos]
  return( list("trainX"=trainX, "trainY"=trainY, "testX"=testX, "testY"=testY))
}


splitData <- function(data, className) {
  ypos <- findPosition(className, colnames(data))
  dataX <- data[,-ypos]
  dataY <- data[, ypos]
  return( list("dataX"=dataX, "dataY"=dataY) )
}


setNA <- function(data, NAname) {
  return( data[data == NAname] <- NA )
}


setNumeric <- function(data, cols) {
  colnames <- colnames(data)
  poss <- findPosition(cols, colnames)
  for ( pos in poss ) { data[,pos] <- as.numeric(data[,pos]) }
  for ( pos in poss ) {
    x <- data[,pos]
    data[,pos] <- x[is.na(x)] <- 0
  }
  return( data )
}


setFactor <- function(data, cols) {
  colnames <- colnames(data)
  poss <- findPosition(cols, colnames)
  for ( pos in poss ) { data[,pos] <- as.factor(data[,pos]) }
  return( data )
}

loadData <- function(savefile, dataname=NULL, msg=NULL) {
  if ( is.null(msg) ) {
    writeLines(paste("Loading",dataname,"from",savefile))
  } else {
    writeLines(paste(msg,"from",savefile))
  }
  lvals <- load(file=savefile)
  return( get(lvals ) )
}

saveData <- function(saveData, savefile, dataname=NULL, msg=NULL) {
  if ( is.null(msg) ) {
    writeLines(paste("Saving",dataname,"to",savefile))
  } else {
    writeLines(paste(msg,"to",savefile))
  }
  save(saveData, file=savefile, compress = T)
}


formatData <- function(dtype) {
  require(data.table)
  formulas     <- list()
  trainingdata <- list()
  testingdata  <- list()
  problemType  <- "binaryClassification"
  
  
  ##########################################################################################################
  #
  # data
  #
  ##########################################################################################################
  if ( dtype == "iris" ) {
    rfile <- "~/mva/data/iris.rData"
    if ( file.exists(rfile) ) {
      writeLines(paste("Loading previous",dtype,"dataset"))
      load(rfile)
    } else {
      writeLines("Using IRIS data")
      data(iris)
      #ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
      ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.1, 0.9))
      train  <- iris[ind==1,]
      test   <- iris[ind==2,]
      trainX <- train[, 1:4]
      testX  <- test[, 1:4]
      trY <- as.factor(train[, 5])
      ttY <- as.factor(test[, 5])
      
      responseNames <- unique(c(levels(trY), levels(ttY)))
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trY, testX, ttY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      trainY <- makeYfac(trY, responseName)
      testY  <- makeYfac(ttY, responseName)
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      colnames(trainingdata[[responseName]])[1] <- responseName
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      formulas[[responseName]]     <- as.formula(paste(responseName,".",sep="~"))
    }
  }
  
  if ( dtype == "Abelone") {
    rfile <- "~/mva/data/abelone.rData"
    if ( file.exists(rfile) ) {
      writeLines(paste("Loading previous",dtype,"dataset"))
      load(rfile)
    } else {
      csvdata <- gsub(".rData", ".data", rfile)
      writeLines(paste("Loading",dtype,"dataset from",csvdata))
      abelonedata <- read.csv(csvdata, sep = ",", header=F)
      cols <- c("Sex", "Length", "Diameter", "Height", "Whole Weight", "Shucked Weight", "Viscera Weight", "Shell Weight", "Rings")
      colnames(abelonedata) <- cols
      cols <- colnames(abelonedata)
      abelonedata <- convertFactors(abelonedata)
      colnames(abelonedata) <- gsub(" ", "", colnames(abelonedata))
      print(colnames(abelonedata))
      ypos = which(colnames(abelonedata) %in% c("Rings"))
      ind <- sample(2, nrow(abelonedata), replace=TRUE, prob=c(0.9, 0.1))
      train  <- abelonedata[ind==1,]
      test   <- abelonedata[ind==2,]
      trainX <- train[,-ypos]
      trainY <- train[, ypos]
      testX  <- test[,-ypos]
      testY  <- test[, ypos]
      responseNames <- c("Rings")
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      trainingdata[[responseName]] <- cbind("Rings"=trainY, trainX)
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      formulas[[responseName]]     <- as.formula(paste("Rings",".",sep="~"))
    }
  }
  
  if ( dtype == "Adult" ) {
    rfile <- "~/mva/data/Adult.rData"
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      colnames <- as.character(read.csv("~/model/adult.names", sep = ":", header=F)[,"V1"])
      train <- read.csv("~/model/adult.data", sep = ",", header=F, col.names = colnames)
      test  <- read.csv("~/model/adult.test", sep = ",", header=F, col.names = colnames)

      posClass <- ">50K"
      newPosClass <- "GT50K"
      train <- downSamp(data = train, responseName = "response", newResponseName = newPosClass)
      test  <- downSamp(data = test,  responseName = "response", newResponseName = newPosClass)
      ## tmp
      keep  <- c(newPosClass, "age", "education.num", "capital.gain", "capital.loss", "hours.per.week")
      train <- train[,keep]
      test  <- test[,keep]

      ypos   <- which(colnames(train) == newPosClass)
      trainX <- train[, -ypos]
      trainY <- train[, ypos]
      testX  <- test[,-ypos]
      testY  <- test[, ypos]

      ## This is specific to Adult dataset      
      trainY <- as.factor(gsub(" ", "", as.character(trainY)))
      testY  <- as.factor(gsub(" ", "", as.character(testY)))
      testY  <- as.factor(gsub("\\.", "", as.character(testY)))
      
      trainY <- makeYfac(trainY, posClass)
      testY  <- makeYfac(testY, posClass)
      
      print(table(trainY))
      print(table(testY))
      
      trainY <- renameLevel(trainY, posClass, newPosClass)
      testY  <- renameLevel(testY, posClass, newPosClass)
      print(table(trainY))
      print(table(testY))

      trainX <- cbind("Source"=replicate(nrow(trainX), 1), trainX)
      testX  <- cbind("Source"=replicate(nrow(testX), 2), testX)
      
      data   <- convertFactors(rbind(trainX, testX))
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      trainX[,"Source"] <- NULL
      testX[,"Source"]  <- NULL
      trainX[,"fnlwgt"] <- NULL
      testX[,"fnlwgt"]  <- NULL
      
      responseNames <- newPosClass
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    
    for ( responseName in responseNames ) {
      formulas[[responseName]] <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]     <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
  }
  
  if ( dtype == "medicaid" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      meddata <- loadData(savefile = rfile, dataname = dtype)
    } else {
      meddata <- fread("~/mva/data/Medicaid.txt", header=T, data.table = F)
      saveData(meddata, savefile = rfile, dataname = dtype)
    }
  }
  
  if ( dtype == "member" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      memdata <- loadData(savefile = rfile, dataname = dtype)
    } else {
      memdata <- fread("~/mva/data/member.txt", header=T, data.table = F)
      saveData(memdata, savefile = rfile, dataname = dtype)
    }
  }
  
  if ( dtype == "um" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      umdata <- loadData(savefile = rfile, dataname = dtype)
    } else {
      umdata <- fread("~/mva/data/um.txt", header=T, data.table = F)
      saveData(umdata, savefile = rfile, dataname = dtype)
    }
  }
  
  if ( dtype == "claims" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      cldata <- loadData(savefile = rfile, dataname = dtype)
    } else {
      files <- list.files("~/mva/data/", pattern = "Claims", full.names = T)
      cldata <- data.frame()
      for ( file in files ) {
        writeLines(paste("  ",file))
        tmp <- fread(file, header=T, data.table = F)
        if ( nrow(cldata) == 0 ) { cldata <- tmp }
        else { cldata <- rbind(cldata, tmp) }
        writeLines(paste("  ",paste(dim(cldata), collapse = " x ")))
      }
      saveData(cldata, savefile = rfile, dataname = dtype)
    }
  }

  if ( dtype == "susy" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      susydata <- fread("~/mva/data/SUSY.csv", header=F, data.table = F)
      cols <- c("Signal", "l1pt", "l1eta", "l1phi", "l2pt", "l2eta", "l2phi", "metpt", "metphi", "metrel", "metax", "MR", "MTR2", "R", "MT2", "SR", "MdeltaR", "dPhiRb", "cosThetaR1")
      colnames(susydata) <- cols
      print(head(susydata))

      ## Define positive class
      posClass <- "Signal"
      
      ## Down sample just to make an even number of signal and background
      susydata <- downSamp(susydata, responseName = posClass)
      
      ## Split data into train/test
      retval <- createTestAndTrain(susydata, posClass, 0.5)
      trainX <- retval[["trainX"]]
      trainY <- retval[["trainY"]]
      testX  <- retval[["testX"]]
      testY  <- retval[["testY"]]
      
      ## Make sure class is named correctly
      trainY <- renameLevel(trainY, posClass)
      testY  <- renameLevel(testY, posClass)
      responseNames <- posClass
      
      ## Convert factors and check for collinearity
      trainX <- cbind("Source"=replicate(nrow(trainX), 1), trainX)
      testX  <- cbind("Source"=replicate(nrow(testX), 2), testX)
      data   <- convertFactors(rbind(trainX, testX))
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      data <- checkFeatures(rbind(trainX, testX), 0.05, removeFeatures = T)
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      trainX[,"Source"] <- NULL
      testX[,"Source"]  <- NULL
      
      ## Save everything
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      formulas[[responseName]]     <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
  }
  
  if ( dtype == "miniboone" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      N <- read.csv("~/mva/data/MiniBooNE_PID.txt", nrows=1, header=F, sep=" ")
      Ns <- N[,"V2"]
      Nb <- N[,"V3"]
      mdata <- fread("~/mva/data/MiniBooNE_PID.txt", header=F, data.table = F)
      signaldata <- head(mdata, n=Ns)
      backgrounddata <- tail(mdata, n=Nb)
      badway <- F
      if ( badway ) {
        mdata <- readLines("~/mva/data/MiniBooNE_PID.txt")
        print(length(mdata))
        tmp <- strsplit(mdata, split = " ")
        tmp <- lapply(tmp, function(x) x[nchar(x) > 0])
        tmp <- tail(tmp, n=-1)
        signaldata <- head(tmp, n=Ns)
        signaldata <- as.data.frame(t(sapply(signaldata, function(x) as.numeric(x))))
        print(dim(signaldata))
        backgrounddata <- tail(tmp, n=Nb)
        backgrounddata <- as.data.frame(t(sapply(backgrounddata, function(x) as.numeric(x))))
        print(dim(backgrounddata))
      }
      
      ## Define positive class
      posClass <- "Signal"
      
      ## Combine signal and background for ease
      mdata <- combineSignalAndBackground(signaldata, backgrounddata, responseName = posClass)

      ## Down sample just to make an even number of signal and background
      mdata <- downSamp(mdata, responseName = posClass)

      ## Split data into train/test
      retval <- createTestAndTrain(mdata, posClass, 0.5)
      trainX <- retval[["trainX"]]
      trainY <- retval[["trainY"]]
      testX  <- retval[["testX"]]
      testY  <- retval[["testY"]]
      
      ## Make sure class is named correctly
      trainY <- renameLevel(trainY, posClass)
      testY  <- renameLevel(testY, posClass)
      responseNames <- posClass
      
      ## Convert factors and check for collinearity
      trainX <- cbind("Source"=replicate(nrow(trainX), 1), trainX)
      testX  <- cbind("Source"=replicate(nrow(testX), 2), testX)
      data   <- convertFactors(rbind(trainX, testX))
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      data <- checkFeatures(rbind(trainX, testX), 0.05, removeFeatures = T)
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      trainX[,"Source"] <- NULL
      testX[,"Source"]  <- NULL
      
      ## Save everything
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      formulas[[responseName]]     <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
  }
  
  if ( dtype == "census" ) {
    rfile <- paste("~/mva/data/",dtype,".rData", sep="")
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      lines <- readLines("~/mva/data/census-income.columns")
      lines <- unlist(lapply(strsplit(lines, "\\)"), function(x) x[1]))
      fields <- unlist(lapply(strsplit(lines, "\\("), function(x) x[2]))
      fields <- unlist(lapply(fields, simpleCap))
      fields <- append(fields, "Something", after = 24)
      fields <- append(fields, "Class")
      censustraindata <- fread(input = "~/mva/data/census-income.data", sep = ",", header = F, showProgress = T, data.table = F)
      censustestdata  <- fread(input = "~/mva/data/census-income.test", sep = ",", header = F, showProgress = T, data.table = F)
      colnames(censustraindata) <- fields
      colnames(censustestdata)  <- fields
      censustraindata <- downSamp(data = censustraindata, responseName = "Class")
      censustestdata  <- downSamp(data = censustestdata, responseName = "Class")
      retval <- splitData(censustraindata, "Class")
      trainX <- retval[["dataX"]]
      trainY <- retval[["dataY"]]
      retval <- splitData(censustestdata, "Class")
      testX  <- retval[["dataX"]]
      testY  <- retval[["dataY"]]
      posClass <- "+ 50000."
      newPosClass <- "Gt50k"
      responseNames <- newPosClass
      trainX <- setFactor(trainX, c("Detailed Industry Recode", "Detailed Occupation Recode"))
      testX  <- setFactor(testX, c("Detailed Industry Recode", "Detailed Occupation Recode"))
      trainY <- renameLevel(trainY, posClass, newPosClass)
      testY  <- renameLevel(testY, posClass, newPosClass)
      print(table(trainY))
      print(table(testY))
      trainX <- cbind("Source"=replicate(nrow(trainX), 1), trainX)
      testX  <- cbind("Source"=replicate(nrow(testX), 2), testX)
      data   <- convertFactors(rbind(trainX, testX))
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      data <- checkFeatures(rbind(trainX, testX), 0.05, removeFeatures = T)
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      trainX[,"Source"] <- NULL
      testX[,"Source"]  <- NULL
      trainX[,"Year"] <- NULL
      testX[,"Year"]  <- NULL
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      formulas[[responseName]]     <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
  }

  if ( dtype == "ad" ) {
    rfile <- "~/mva/data/ad.rData"
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      addata <- fread(input = "~/mva/data/ad.data", sep = ",", header = F, showProgress = T, data.table = F)
      cols <- c("height", "width", "aratio", "local")
      cols <- c(cols, paste("url", as.character(seq(457)), sep=""))
      cols <- c(cols, paste("origurl", as.character(seq(495)), sep=""))
      cols <- c(cols, paste("ancurl", as.character(seq(472)), sep=""))
      cols <- c(cols, paste("alt", as.character(seq(111)), sep=""))
      cols <- c(cols, paste("caption", as.character(seq(19)), sep=""))
      cols <- c(cols, "class")
      colnames(addata) <- cols
      
      addata <- downSamp(data = addata, responseName = "class")
      
      retval <- createTestAndTrain(addata, "class", 0.5)
      trainX <- retval[["trainX"]]
      trainY <- retval[["trainY"]]
      testX  <- retval[["testX"]]
      testY  <- retval[["testY"]]
      posClass <- "ad."
      newPosClass <- "Ad"
      responseNames <- newPosClass
      trainX <- setNumeric(trainX, c("height", "width", "aratio"))
      testX  <- setNumeric(testX, c("height", "width", "aratio"))
      trainY <- renameLevel(trainY, posClass, newPosClass)
      testY  <- renameLevel(testY, posClass, newPosClass)
      trainX <- cbind("Source"=replicate(nrow(trainX), 1), trainX)
      testX  <- cbind("Source"=replicate(nrow(testX), 2), testX)
      data   <- convertFactors(rbind(trainX, testX))
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      data <- checkFeatures(rbind(trainX, testX), 0.05, removeFeatures = T)
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      trainX[,"Source"] <- NULL
      testX[,"Source"]  <- NULL
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      formulas[[responseName]]     <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
  }
    
  if ( dtype == "housing" ) {
    rfile <- "~/mva/data/housing.rData"
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      housingdata <- read.fwf("~/mva/data/housing.data.txt", widths=c(9, 8, 6, 5, 8, 7, 7, 9, 4, 7, 6, 6, 8, 8))
      cols <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
      colnames(housingdata) <- cols
      retval <- createTestAndTrain(housingdata, "MEDV", 0.5)
      trainX <- retval[["trainX"]]
      trainY <- retval[["trainY"]]
      testX  <- retval[["testX"]]
      testY  <- retval[["testY"]]
      responseNames <- c("MEDV")
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    for ( responseName in responseNames ) {
      formulas[[responseName]]     <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]  <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
    print(class(testY))
  }
  
  if ( dtype == "ecoli" ) {
    rfile <- "~/mva/data/ecoli.rData"
    if ( file.exists(rfile) ) {
      load(rfile)
    } else {
      ecolidata <- read.fwf("~/mva/data/ecoli.data", widths = c(12, 6, 6, 6, 6, 6, 6, 6, 6), header=F)
      ecolidata[,"V1"] <- gsub(" ", "", as.character(ecolidata[,"V1"]))
      ecolidata[,"V9"] <- as.factor(gsub(" ", "", as.character(ecolidata[,"V9"])))
      cols <- c("sequence", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2", "class")
      colnames(ecolidata) <- cols
      ecolidata[,"lip"] <- as.factor(ecolidata[,"lip"])
      levels(ecolidata[,"lip"]) <- c("LowScore", "HighScore")
      ecolidata[,"chg"] <- as.factor(ecolidata[,"chg"])
      levels(ecolidata[,"chg"]) <- c("NoCharge", "Charge")
      print(summary(ecolidata))
      f()

      ypos   <- which(colnames(train) == newPosClass)
      trainX <- train[, -ypos]
      trainY <- train[, ypos]
      testX  <- test[,-ypos]
      testY  <- test[, ypos]
      
      ## This is specific to Adult dataset      
      trainY <- as.factor(gsub(" ", "", as.character(trainY)))
      testY  <- as.factor(gsub(" ", "", as.character(testY)))
      testY  <- as.factor(gsub("\\.", "", as.character(testY)))
      
      trainY <- makeYfac(trainY, posClass)
      testY  <- makeYfac(testY, posClass)
      
      print(table(trainY))
      print(table(testY))
      
      trainY <- renameLevel(trainY, posClass, newPosClass)
      testY  <- renameLevel(testY, posClass, newPosClass)
      print(table(trainY))
      print(table(testY))
      
      trainX <- cbind("Source"=replicate(nrow(trainX), 1), trainX)
      testX  <- cbind("Source"=replicate(nrow(testX), 2), testX)
      
      data   <- convertFactors(rbind(trainX, testX))
      trainX <- data[which(data[,"Source"] == 1),]
      testX  <- data[which(data[,"Source"] == 2),]
      trainX[,"Source"] <- NULL
      testX[,"Source"]  <- NULL
      trainX[,"fnlwgt"] <- NULL
      testX[,"fnlwgt"]  <- NULL
      
      responseNames <- newPosClass
      writeLines(paste("Saving test/train to",rfile))
      save(trainX, trainY, testX, testY, responseNames, file=rfile)
      formatData(dtype)
    }
    
    for ( responseName in responseNames ) {
      formulas[[responseName]] <- as.formula(paste(responseName,".",sep="~"))
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]     <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
  }
  
  if ( dtype == "image" ) {
    writeLines("Using IMAGE data")
    if ( file.exists("~/model/image.rData") ) {
      load("~/model/image.rData")
    } else {
      cols <- "REGION-CENTROID-COL,REGION-CENTROID-ROW,REGION-PIXEL-COUNT,SHORT-LINE-DENSITY-5,SHORT-LINE-DENSITY-2,VEDGE-MEAN,VEDGE-SD,HEDGE-MEAN,HEDGE-SD,INTENSITY-MEAN,RAWRED-MEAN,RAWBLUE-MEAN,RAWGREEN-MEAN,EXRED-MEAN,EXBLUE-MEAN,EXGREEN-MEAN,VALUE-MEAN,SATURATION-MEAN,HUE-MEAN"
      cols <- strsplit(cols, split = ",")[[1]]
      cols <- c("Class", cols)
      train <- read.csv(file = "~/model/segmentation.data.txt", header = F, sep=",", col.names = cols)
      test  <- read.csv(file = "~/model/segmentation.test.txt", header = F, sep=",", col.names = cols)
      save(train, test, file="~/model/image.rData")
    }
    train <- train[, -c(2, 3, 4)]
    trainNum <- train
    trainNum[,1] <- as.numeric(train[,1])
    test <- test[, -c(2, 3, 4)]
    trainX <- train[, -1]
    testX  <- test[, -1]
    retvals <- removeCollinearFeatures(trainX, testX)
    trainX  <- retvals[["trainX"]]
    testX   <- retvals[["testX"]]
    trainY <- train[, 1]
    testY  <- test[, 1]
    train <- cbind(trainY, trainX)
    test  <- cbind(testY, testX)
    colnames(train)[1] <- "Class"
    colnames(test)[1]  <- "Class"
    
    formula <- as.formula("Class~.")
    formulas <- list()
    trainingdata <- list()
    testingdata <- list()
    responseNames <- unique(train[, 1])
    responseNames <- c("CEMENT")
    for ( responseName in responseNames ) {
      formulas[[responseName]] <- as.formula(paste(responseName,".",sep="~"))
      trainY <- makeYfac(train[, 1], responseName)
      testY  <- makeYfac(test[, 1], responseName)
      trainingdata[[responseName]] <- cbind(trainY, trainX)
      testingdata[[responseName]]     <- list("Y"=testY, "X"=testX)
      colnames(trainingdata[[responseName]])[1] <- responseName
    }
    
    if ( F ) {    
      responseName <- "BRICKFACE"
      formulaFac <- as.formula(paste(responseName,".",sep="~"))
      trainYnum  <- trainNum[,1]
      testYnum   <- testNum[,1]
      trainYfac <- makeYfac(trainY, responseName)
      testYfac  <- makeYfac(testY, responseName)
      trainFac <- cbind(trainYfac, trainX)
      colnames(trainFac)[1] <- responseName
      testFac <- cbind(testYfac, testX)
      colnames(testFac)[1] <- responseName
    }
  }
  
  if ( length(formulas) == 0 ) { 
    writeLines(paste("No formulas given for",dtype))
    stop()
  } else if ( length(trainingdata) == 0 ) { 
    writeLines(paste("No testing data given for",dtype))
    stop()
  } else if ( length(testingdata) == 0 ) { 
    writeLines(paste("No testing data given for",dtype))
    stop()
  } else if ( length(responseNames) != length(formulas) ) { 
    writeLines(paste("Length of response names does not equal length of formulas",dtype))
    stop()
  }
  
  if ( class(trainY) == "numeric" | class(trainY) == "integer" ) { problemType <- "linearRegression" }
  writeLines(paste("Problem Type:",problemType))
  writeLines(paste("  Found",length(formulas),"response name(s)"))
  for ( responseName in responseNames ) {
    writeLines(paste("    -->",responseName,": Train :",paste(dim(trainingdata[[responseName]]), collapse = " x ")))
    writeLines(paste("    -->",responseName,":  Test :",paste(dim(testingdata[[responseName]][["X"]]), collapse = " x ")))
  }
  
  return( list("formulas"=formulas, "trainingData"=trainingdata, "testingData"=testingdata, "responseNames"=responseNames, "problemType"=problemType) )
}