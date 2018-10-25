mvadir <- file.path(getwd(), "mva")
stopifnot(dir.exists(mvadir))

files <- c("helper.R", "dataOps.R", "fileio.R", "logger.R")
for ( file in files ) {
  fname <- file.path(mvadir, file)
  stopifnot(file.exists(fname))
  source(fname)
}


############################################################################################################
# Get FWF Split Data
############################################################################################################
getFWFSplitData <- function(ifile, sep=" ", ...) {
  options    <- unlist(list(...))
  debug      <- any(ifelse(c("debug","Debug") %in% options, T, F))
  
  line <- readLines(ifile, n = 1)
  sepL <- getSepLength(line = line, sep = sep, options)
  
  print(line)
  print(sepL)
  f()
}



############################################################################################################
# Format and save data
############################################################################################################
getDataDir <- function(fname) {
  datadir <- file.path(mvadir, "data", fname)
  if ( !dir.exists(datadir) ) { stop(paste(fname,"directory does not exist.")) }
  return( datadir )
}

getDataName <- function(fname) {
  datadir <- getDataDir(fname)
  rData   <- file.path(datadir, paste(fname,"rData", sep="."))
  return( rData )
}

saveData <- function(fdata, fname, colnames = NULL, rData) {
  writeLines(paste("Read in",nrow(fdata),"rows and",ncol(fdata),"cols of",fname,"data."))
  if ( is.null(colnames) ) {
    colnames(fdata) <- make.names(colnames(fdata))
  } else {
    colnames(fdata) <- make.names(colnames)
  }
  writeLines(paste("Saving",fname,"data to",rData))
  save(fdata, file = rData, compress = T)  
}

############################################################################################################
# Communities
############################################################################################################
getCommunitiesData         <- function() {
  fname   <- "communities"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="ViolentCrimesPerPop", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "communities.names")
    tmp      <- read.csv("~/Documents/mva/data/communities/communities.names", header = F, stringsAsFactors = F, sep=":")[,"V1"]
    names    <- trimws(gsub("-", " ", tmp))
    names    <- names[-7] ## Strangeness with processing
    ifile    <- file.path(datadir, "communities.data")
    idata    <- fread("~/Documents/mva/data/communities/communities.data", header = F, data.table = F)
    colnames(idata) <- names
    factors  <- c("state", "county", "community", "communityname", "LemasGangUnitDeploy")
    for ( col in factors ) { idata[,col] <- as.factor(idata[,col]) }
    chars    <- colnames(idata)[sapply(colnames(idata), function(x) class(idata[,x])) == "character"]
    for ( col in chars ) { idata[,col] <- as.numeric(idata[,col]) }
    rms <- c("county", "community", "communityname", "fold")
    pos <- loc(rms, colnames(idata))
    idata <- idata[,-pos]
    saveData(fdata = idata, fname = fname, colnames = NULL, rData = rData)
    getCommunitiesData()
  }
}


############################################################################################################
# Miniboone
############################################################################################################
getMiniBooneData         <- function() {
  fname   <- "miniboone"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="class", "target"="signal", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "MiniBooNE_PID.txt")
    N <- read.csv(ifile, nrows=1, header=F, sep=" ")
    Ns <- N[,"V2"]
    Nb <- N[,"V3"]
    mdata <- fread(ifile, header=F, data.table = F, showProgress = T)
    signaldata <- head(mdata, n=Ns)
    signaldata[,"class"] <- as.factor("signal")
    backgrounddata <- tail(mdata, n=Nb)
    backgrounddata[,"class"] <- as.factor("background")
    miniboonedata   <- rbind(signaldata, backgrounddata)
    saveData(fdata = miniboonedata, fname = fname, colnames = NULL, rData = rData)
    getMiniBooneData()
  }
}


############################################################################################################
# MAGIC
############################################################################################################
getMagicData         <- function() {
  fname   <- "magic"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="class", "target"="g", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile     <- file.path(datadir, "magic04.names")
    names     <- as.character(read.csv(ifile, header=F, sep=":")[,"V1"])    
    snames    <- sapply(names, function(x) strsplit(x, split = ". "))    
    cols      <- unname(sapply(snames, function(x) trimws(tail(x, n=1))))
    ifile     <- file.path(datadir, "magic04.data")
    magicdata <- fread(ifile, data.table = F, header=T, showProgress = T)
    saveData(fdata = magicdata, fname = fname, colnames = cols, rData = rData)
    getMagicData()
  }
}


############################################################################################################
# NewCo
############################################################################################################
getNewCoData         <- function() {
  fname   <- "newco"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Rating", "target"=1, "data"=get0("fdata")) )
  }
  else {
    fpath <- file.path(datadir, "newcoRaw.rData")
    lvals <- load(fpath)
    newco[,"Unique.ID"] <- NULL
    newco[,"Rating"]    <- as.numeric(newco[,"Rating"])
    newco[,"NoEval"]    <- as.numeric(newco[,"NoEval"])
    newco[,"NumJobs"]   <- as.numeric(newco[,"NumJobs"])
    newco[,"AvgTime"]   <- as.numeric(newco[,"AvgTime"])
    newco <- newco[!(is.na(newco[,"Rating"])),]
    newco[,"Rating"]    <- ifelse(newco[,"Rating"] < 2.0, 1, 0)
    cols  <- colnames(newco)
    saveData(fdata = newco, fname = fname, colnames = cols, rData = rData)
    getNewCoData()
  }
}


############################################################################################################
# EWI
############################################################################################################
getEWIData         <- function() {
  fname   <- "ewi"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="TARGET", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    fdata                   <- fread("~/ewi/aa-ewiforecast-disc/data/CharlieEWIfeatures.csv", data.table = F)
    fdata[,"incr_yrmo"]     <- as.Date(as.yearmon(as.character(fdata[,"incr_yrmo"]), "%Y%m"))
    fdata[,"raw_paid_days"] <- as.numeric(noquote(gsub(",", "", fdata[,"raw_paid_days"])))
    fdata[,"fnl_alwd_amt"]  <- as.numeric(noquote(gsub(",", "", fdata[,"fnl_alwd_amt"])))
    fdata[,"fnl_alwd_amt"]  <- fdata[,"fnl_alwd_amt"] / 1e6
    fdata[,"TARGET"]        <- fdata[,"fnl_alwd_amt"]
    fdata[,"fnl_alwd_amt"]  <- NULL
    fdata[,"zlag_alwd_amt"] <- fdata[,"zlag_alwd_amt"] / 1e6
    #fdata[,"TRAIN"]        <- ifelse(fdata[,"tst_trn"] == "TRN", 1, 0)
    fdata[,"isTrain"]       <- fdata[,"tst_trn"]
    rownames(fdata) <- make.names(paste(fdata[,"Brand_st"], fdata[,"rpt_grp"],fdata[,"incr_yrmo"],fdata[,"isTrain"], sep = "_"))
    fdata[,"Brand_st"]  <- NULL
    fdata[,"rpt_grp"]   <- NULL
    fdata[,"tst_trn"]   <- NULL
    fdata[,"incr_yrmo"] <- NULL
    fdata[is.na(fdata)] <- 0
    cols <- colnames(fdata)
    saveData(fdata = fdata, fname = fname, colnames = cols, rData = rData)
    getEWIData()
  }
}

############################################################################################################
# Car
############################################################################################################
getCarData         <- function() {
  fname   <- "car"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="class", "target"="unacc", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile     <- file.path(datadir, "car.names")
    cols      <- as.character(read.csv(ifile, header=F, sep=" ")[,"V4"])
    ifile     <- file.path(datadir, "car.data")
    cardata   <- fread(ifile, data.table = F, header=F, showProgress = T)
    saveData(fdata = cardata, fname = fname, colnames = cols, rData = rData)
    getCarData()
  }
}

############################################################################################################
# Credit
############################################################################################################
getCreditData         <- function() {
  fname   <- "credit"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="default.payment.next.month", "target"="1", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile      <- file.path(datadir, "credit.csv")
    creditdata <- fread(file = ifile, data.table = F)
    saveData(fdata = creditdata, fname = fname, colnames = NULL, rData = rData)
    getCreditData()
  }
}

############################################################################################################
# Bank
############################################################################################################
getBankData         <- function() {
  fname   <- "bank"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="y", "target"="yes", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "bank-additional-full.csv")
    bankdata <- fread(ifile, data.table = F, sep=";", header=T)
    saveData(fdata = bankdata, fname = fname, colnames = NULL, rData = rData)
    getBankData()
  }
}

############################################################################################################
# Yeast
############################################################################################################
getYeastData         <- function() {
  fname   <- "yeast"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="class", "target"="CYT", "data"=get0("yeastdata")) )
  }
  else {
    require(data.table)
    colfile   <- file.path(datadir, "yeast.colnames")
    cols      <- as.character(read.csv(colfile, sep = ":", header=F)[,1])
    ifile     <- file.path(datadir, "yeast.data")
    yeastdata <- fread(ifile, data.table = F, header=T)
    yeastdata <- yeastdata[,-1]
    saveData(fdata = yeastdata, fname = fname, colnames = cols, rData = rData)
    getYeastData()
  }
}

############################################################################################################
# Abalone
############################################################################################################
getAbaloneData         <- function() {
  fname   <- "abalone"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Sex", "target"="M", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    colfile   <- file.path(abalonedir, "abalone.names")
    cols      <- as.character(read.csv(colfile, sep = "\t", header=F)[,2])
    ifile     <- file.path(abalonedir, "abalone.data")
    abalonedata <- fread(ifile, data.table = F, header=T)
    saveData(fdata = abalonedata, fname = fname, colnames = cols, rData = rData)
    getAbaloneData()
  }
}

############################################################################################################
# Customer
############################################################################################################
getCustomerData         <- function() {
  fname   <- "customer"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Channel", "target"="1", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile        <- file.path(datadir, "Wholesale_Customers_Data.csv")
    customerdata <- fread(ifile, data.table = F, header=T, showProgress = T)
    tmp <- customerdata[,"Channel"]
    tmp[tmp == 2] <- 0
    customerdata[,"Channel"] <- tmp
    saveData(fdata = customerdata, fname = fname, colnames = NULL, rData = rData)
    getCustomerData()
  }
}

############################################################################################################
# Adult
############################################################################################################
getAdultData         <- function() {
  fname   <- "adult"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="target", "target"=">50K", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    anamesfile  <- file.path(datadir, "adult.names")
    anames      <- read.csv(anamesfile, sep=":", header = F)[,"V1"]
    trainfile   <- file.path(datadir, "adult.data.txt")
    traindata   <- fread(trainfile, data.table = F)
    colnames(traindata) <- anames
    testfile    <- file.path(datadir, "adult.test.txt")
    testdata    <- fread(trainfile, data.table = F)
    colnames(testdata) <- anames
    adultdata   <- rbind(traindata, testdata)
    adultdata   <- adultdata[,-3] # remove fnlwgt
    saveData(fdata = adultdata, fname = fname, colnames = NULL, rData = rData)
    getAdultData()
  }
}

############################################################################################################
# Wine
############################################################################################################
getWineData         <- function() {
  fname   <- "wine"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Wine", "target"="red", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    redfile   <- file.path(datadir, "winequality-red.csv")
    red       <- fread(redfile, data.table = F)
    red[,"Wine"] <- as.factor("red")
    whitefile <- file.path(datadir, "winequality-white.csv")
    white     <- fread(redfile, data.table = F)
    white[,"Wine"] <- as.factor("white")
    winedata  <- rbind(red, white)
    saveData(fdata = winedata, fname = fname, colnames = NULL, rData = rData)
    getWineData()
  }
}

############################################################################################################
# Colon
############################################################################################################
getColonoscopyData <- function() {
  fname   <- "colon"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="consensus", "target"="1", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    gfile  <- file.path(datadir, "green.csv")
    green  <- fread(gfile, data.table = F)
    hfile  <- file.path(datadir, "hinselmann.csv")
    hinse  <- fread(hfile, data.table = F)
    sfile  <- file.path(datadir, "schiller.csv")
    schil  <- fread(sfile, data.table = F)
    colondata <- rbind(green, hinse, schil)
    colondata <- colondata[,-c(63:68)] ## removing alternative targets    
    saveData(fdata = colondata, fname = fname, colnames = NULL, rData = rData)
    getColonoscopyData()
  }
}
    
############################################################################################################
# Cancer
############################################################################################################
getCervicalCancer <- function() {
  fname   <- "cancer"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Biopsy", "target"="1", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile  <- file.path(datadir, "risk_factors_cervical_cancer.csv")
    cancerdata <- fread(ifile, data.table = F, stringsAsFactors = F)
    indx <- sapply(cancerdata, is.character)
    cancerdata[indx] <- lapply(cancerdata[indx], function(x) as.numeric(x))
    cancerdata <- cancerdata[,-c(33,34,35)] ## removing alternative targets
    saveData(fdata = cancerdata, fname = fname, colnames = NULL, rData = rData)
    getCervicalCancer()
  }
}

############################################################################################################
# Facebook
############################################################################################################
getFacebookData <- function() {
  fname   <- "fb"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Target.Variable", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "train.csv")
    fbdata   <- fread(ifile, data.table = F, stringsAsFactors = F)
    ifile    <- file.path(datadir, "features.csv")
    features <- read.csv(file = "~/Documents/mva/data/fb/features.csv", header = F)[,"V1"]
    features <- as.character(features)
    colnames(fbdata) <- make.names(features, unique = T)
    pos <- grep("Category", colnames(fbdata))
    for ( i in pos ) { fbdata[,i] <- as.factor(fbdata[,i]) }
    fbdata     <- sampleData(fdata = fbdata, sFraction = 0.25)
    saveData(fdata = fbdata, fname = fname, colnames = NULL, rData = rData)
    getFacebookData()
  }
}

############################################################################################################
# Blog
############################################################################################################
getBlogData <- function() {
  fname   <- "blog"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="target", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "blogData_train.csv")
    blogdata <- fread(ifile, data.table = F, stringsAsFactors = F)
    feats    <- c("n", "nb24", "nb48", "na24", "dn")
    pars     <- c("avg", "sd", "min", "max", "med")
    ptypes   <- c("Comments", "Links")
    features <- c()
    for ( ptype in ptypes ) {
      for ( feat in feats ) {
        for ( par in pars ) { features <- c(features, paste(feat,ptype,par,sep="")) }
      }
    }
    features <- c(features, paste(feats,"Commnets",sep=""), paste(feats,"Links",sep=""))
    features <- c(features, "tPublication", "lenPublication", paste("word",seq(200),sep = ""))
    features <- c(features, paste("bt",seq(7),sep=""), paste("pt",seq(7),sep=""))
    features <- c(features, "nParents", "minParCom", "maxParCom", "avgParCom", "target")
    colnames(blogdata) <- make.names(features, unique = T)
    saveData(fdata = blogdata, fname = fname, colnames = NULL, rData = rData)
    getBlogData()
  }
}

############################################################################################################
# Generated
############################################################################################################
getGeneratedData <- function() {
  fname   <- "gen"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="value", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    maxVal <- 5000
    x1 <- rnorm(maxVal, mean = seq(1), sd = 2)
    x2 <- rnorm(maxVal, mean = seq(-1000,0), sd = 20)
    x3 <- rpois(maxVal, 15)
    x4 <- rnorm(maxVal, mean = seq(-10000, 10000, length.out = 1000), sd = 2000)
    y1 <- 25*x1 - 20*x2 - x3 + x4
    fdata <- data.frame('value'=y1, x1, x2, x3, x4)
    saveData(fdata = fdata, fname = fname, colnames = NULL, rData = rData)
    getGeneratedData()
  }
}


############################################################################################################
# Poly
############################################################################################################
getPolyData <- function() {
  fname   <- "poly"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="value", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "poly.dat")
    pdata    <- fread(ifile, data.table = F, stringsAsFactors = F)
    colnames(pdata) <- c("value", "x")
    saveData(fdata = pdata, fname = fname, colnames = NULL, rData = rData)
    getPolyData()    
  }
}


############################################################################################################
# Rational
############################################################################################################
getRationalData <- function() {
  fname   <- "rational"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="value", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "rational.dat")
    pdata    <- fread(ifile, data.table = F, stringsAsFactors = F)
    colnames(pdata) <- c("value", "x")
    saveData(fdata = pdata, fname = fname, colnames = NULL, rData = rData)
    getRationalData()    
  }
}


############################################################################################################
# Yearly Predict
############################################################################################################
getYearlyPredictData <- function() {
  fname   <- "yearpredict"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="year", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile    <- file.path(datadir, "yearpredict.dat")
    pdata    <- fread(ifile, data.table = F, stringsAsFactors = F)
    colnames(pdata) <- c("year", make.names(paste("timbreavg",seq(12))), make.names(paste("timbrecov",seq(78))))
    saveData(fdata = pdata, fname = fname, colnames = NULL, rData = rData)
    getYearlyPredictData()    
  }
}




############################################################################################################
# CBM
############################################################################################################
getCBMData <- function() {
  fname   <- "cbm"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="Biopsy", "target"="1", "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile  <- file.path(datadir, "data.txt")
    cbmdata <- fread(ifile, data.table = F, stringsAsFactors = F)
    stop("Not working yet")
    indx <- sapply(cancerdata, is.character)
    cancerdata[indx] <- lapply(cancerdata[indx], function(x) as.numeric(x))
    cancerdata <- cancerdata[,-c(33,34,35)] ## removing alternative targets
    saveData(fdata = cancerdata, fname = fname, colnames = NULL, rData = rData)
    getCervicalCancer()
  }
}

############################################################################################################
# Anthem
############################################################################################################
getAnthemData <- function() {
  loginfo("Getting Antem Data")
  fname   <- "anthem"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="TOTALCOST", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile  <- file.path("/Users/tgadfort/Google Drive", "20160930.rData")
    lvals  <- load(ifile)
    df     <- df[,-1] # drop MCID
    keep   <- c(grep("MBRAGE", colnames(df)), grep("AMT", colnames(df)), grep("TOTALCOST", colnames(df)))
    keep   <- c("MBRAGE", "C1NMEDICINEAMT", "C1NEVALUATIONANDMANAGEMENTAMT", "CANCEROTEXCLSNAMT", "CANCEREXCLSNAMT", "MLSAAVGHSHLDINCMAMT", "MLSAAVGHOUSGVALAMT", "MLSAMEDNHSHLDINCMAMT", "MLSAMEDNINCMPERPRSNAMT", "MLSAHOUSVALAMT", "ELIXHAUSERCONGESTIVEHEARTFAILUREAMT", "ELIXHAUSERCARDIACARRHYTHMIASAMT", "ELIXHAUSEREXTRAAMT", "ELIXHAUSERSOLIDTUMORWITHOUTMETASTASISAMT", "FLAGSASCHYPERTENSIONAMT", "FLAGSASCALLAMT", "MDCLAMTALLFORECAST6MONTH", "MDCLAMTHALFFORECAST6MONTH", "MDCLAMTMIDFORECAST6MONTH", "RXAMTALLFORECAST6MONTH", "RXAMTHALFFORECAST6MONTH", "RXAMTMIDFORECAST6MONTH", "SPECIALITYHOMEHEALTHVOLUNTARYHEALTHCHARITYAMT", "SPECIALITYHOSPITALAMT", "SPECIALITYOPHTHALMOLOGYAMT", "SPECIALITYCARDIOLOGYAMT", "SPECIALITYMEDICALONCOLOGYAMT", "SPECIALITYPRIMARYCAREAMT", "SPECIALITYMULTISPECIALTYCLINICGROUPPRACTICEAMT", "SPECIALITYRADIOLOGYAMT", "MBRANNUALIZEDTOTALCOST", "TOTALCOST")
    df     <- df[,keep]
    df     <- df[df[,"TOTALCOST"] > 100 & df[,"TOTALCOST"] < 1000,]
    df     <- sampleData(fdata = df, sFraction = 0.25)
    saveData(fdata = df, fname = fname, colnames = NULL, rData = rData)
    getAnthemData()
  }
}

############################################################################################################
# Crime
############################################################################################################
getCrimeData <- function() {
  loginfo("Getting Crime Data")
  fname   <- "crime"
  datadir <- getDataDir(fname)
  rData   <- getDataName(fname)
  if ( file.exists(rData) ) {
    load(file = rData)
    return( list("targetcol"="murders", "target"=NULL, "data"=get0("fdata")) )
  }
  else {
    require(data.table)
    ifile     <- file.path(datadir, "crime.data")
    crimedata <- fread(ifile, data.table = F, stringsAsFactors = F)
    ifile  <- file.path(datadir, "crime.names")
    cols   <- readLines(ifile)
    cols   <- cols[unlist(lapply(cols, nchar)) > 0]
    cols   <- unlist(lapply(cols, function(x) strsplit(x = x, split = ":", fixed = T)[[1]][1]))
    cols   <- unlist(lapply(cols, function(x) strsplit(x = x, split = " ", fixed = T)[[1]][2]))
    colnames(crimedata) <- cols
    crimedata <- crimedata[,-c(1,2,3,4,5)]
    crimedata <- setCharacterToNumeric(crimedata)
    saveData(fdata = crimedata, fname = fname, colnames = NULL, rData = rData)
    getCrimeData()
  }
}

############################################################################################################
# Australian
############################################################################################################
getAustralianData   <- function() {
  require(deepboost)
  fdata <- deepboost::australian
  return( list("targetcol"="X1", "target"="1", "data"=get0("fdata")) )
}

############################################################################################################
# Churn
############################################################################################################
getChurnData   <- function() {
  require(C50)
  data("churn")
  fdata <- rbind(churnTrain, churnTest)
  pos <- loc("state", colnames(fdata))
  fdata <- fdata[,-pos] # remove state
  return( list("targetcol"="churn", "target"="yes", "data"=get0("fdata")) )
}

############################################################################################################
# Iris
############################################################################################################
getIrisData         <- function() {
  return( list("targetcol"="Species", "target"="setosa", "data"=get0("iris")) )
}

############################################################################################################
# Ruspini
############################################################################################################
getRuspiniData         <- function() {
  data(ruspini, package = "cluster")
  return( list("targetcol"=NULL, "target"=NULL, "data"=get0("ruspini")) )
}

############################################################################################################
# Boston
############################################################################################################
getBostonData         <- function() {
  require(MASS)
  data("Boston")
  return( list("targetcol"="crim", "target"=NULL, "data"=get0("Boston")) )
}

getSegmentationData <- function() { 
  require(caret)
  data("segmentationData")
  segmentationData <- segmentationData[,-c(1,2)]
  return( list("targetcol"="Class", "target"="WS", "data"=get0("segmentationData")) )
}
getBreastCancerData <- function() { 
  require(mlbench)
  data("BreastCancer")
  BreastCancer <- BreastCancer[,-1]
  return( list("targetcol"="Class", "target"="malignant", "data"=get0("BreastCancer")) )
}
getGlassData <- function() { 
  require(mlbench)
  data("Glass")
  return( list("targetcol"="Type", "target"="1", "data"=get0("Glass")) )
}
getPimaData <- function() { 
  require(mlbench)
  data("PimaIndiansDiabetes")
  return( list("targetcol"="diabetes", "target"="pos", "data"=get0("PimaIndiansDiabetes")) )
}
getHouseVotesData <- function() { 
  require(mlbench)
  data("HouseVotes84")
  return( list("targetcol"="Class", "target"="democrat", "data"=get0("HouseVotes84")) )
}

loadData <- function(name) {
  loginfo(paste("Getting data with name:",name))

  retval <- NULL
  if ( name == "iris" ) { retval <- getIrisData() }
  if ( name == "segm" ) { retval <- getSegmentationData() }
  if ( name == "canc" ) { retval <- getBreastCancerData() }
  if ( name == "pima" ) { retval <- getPimaData() }
  if ( name == "glss" ) { retval <- getGlassData() }
  if ( name == "hvot" ) { retval <- getHouseVotesData() }
  if ( name == "wine" ) { retval <- getWineData() }
  if ( name == "cerv" ) { retval <- getCervicalCancer() }
  if ( name == "coln" ) { retval <- getColonoscopyData() }
  if ( name == "adlt" ) { retval <- getAdultData() }
  if ( name == "bank" ) { retval <- getBankData() }
  if ( name == "yest" ) { retval <- getYeastData() }
  if ( name == "abel" ) { retval <- getAbaloneData() }
  if ( name == "cust" ) { retval <- getCustomerData() }
  if ( name == "mini" ) { retval <- getMiniBooneData() }
  if ( name == "cred" ) { retval <- getCreditData() }
  if ( name == "magi" ) { retval <- getMagicData() }
  if ( name == "car"  ) { retval <- getCarData() }
  if ( name == "aust" ) { retval <- getAustralianData() }
  if ( name == "chrn" ) { retval <- getChurnData() }
  if ( name == "bost" ) { retval <- getBostonData() }
  if ( name == "fb" )   { retval <- getFacebookData() }
  if ( name == "ewi" )  { retval <- getEWIData() }
  if ( name == "nwco" ) { retval <- getNewCoData() }
  if ( name == "blog" ) { retval <- getBlogData() }
  if ( name == "cbm" )  { retval <- getCBMData() }
  if ( name == "comm" ) { retval <- getCommunitiesData() }
  if ( name == "anth" ) { retval <- getAnthemData() }
  if ( name == "gen" )  { retval <- getGeneratedData() }
  if ( name == "poly" ) { retval <- getPolyData() }
  if ( name == "rusp" ) { retval <- getRuspiniData() }
  if ( name == "rat" )  { retval <- getRationalData() }
  if ( name == "year" ) { retval <- getYearlyPredictData() }
  if ( name == "crim" ) { retval <- getCrimeData() }
  if ( is.null(retval) ) { stop(paste("Name:",name,"was not recognized")) }

  targetcol <- retval[["targetcol"]]
  target    <- retval[["target"]]
  ptype     <- NULL
  if ( is.null(targetcol) & is.null(target) ) {
    ptype <- "cluster"
    loginfo(paste("Found data with name:",name,", no target, and size:", getDimStr(retval[["data"]])))
  } else if ( is.null(target) ) {
    ptype <- "linearRegression"
    loginfo(paste("Found data with name:",name,", target column:",retval[["targetcol"]],", and size:", getDimStr(retval[["data"]])))
  } else {
    ptype <- "binaryClassification"
    loginfo(paste("Found data with name:",name,", target:",retval[["target"]],", and size:", getDimStr(retval[["data"]])))
  }
    
  fdata <- retval[["data"]]
  logdebug(paste("  Got data with",nrow(fdata),"rows and",ncol(fdata),"columns."))
  logdebug(paste("  Assigning this as a",ptype,"problem type."))

  return( retval )
}


getTrainData <- function(df) {
  pos   <- loc("isTrain", colnames(df))
  if ( is.null(pos) ) { 
    train <- df
    return( train )
  }
  train <- df[df[,"isTrain"] == T,]
  train <- train[,-pos]
  return( train )
}

getTestData <- function(df, target = "TARGET") {
  pos1 <- loc("isTrain", colnames(df))
  if ( is.null(pos1) ) {
    test <- df
  } else {
    test <- df[df[,"isTrain"] == F,]
    test <- test[,-pos1]
  }
  pos2 <- loc(target, colnames(test))
  if ( is.null(pos2) ) { return( test ) }
  if ( ncol(test) == 2 ) {
    colname <- rmElement(target, colnames(test))
    test <- data.frame(test[,-pos2])
    colnames(test) <- colname
  } else {
    test <- test[,-pos2]
  }
    
  return( test )
}

getTestTarget <- function(df, target = "TARGET") {
  pos1 <- loc("isTrain", colnames(df))
  if ( is.null(pos1) ) {
    test <- df
  } else {
    test <- df[df[,"isTrain"] == F,]
    test <- test[,-pos1]
  }
  pos <- loc(target, colnames(test))
  if ( is.null(pos) ) { return( NULL ) }
  tdata <- test[,pos]
  return( tdata )
}