saveYaml <- function(ydata, filename="dummy.yaml") {
  require(yaml)
  writeLines("Saving data to",filename)
  writeLines(as.yaml(ydata, column.major = F),con = filename)
}

loadYaml <- function(filename="dummy.yaml") {
  x=1
}


isNotSet <- function(...) {
  boolOperation <- "or"
  names <- list(...)
  if ( boolOperation == "or" ) {
    for ( name in names ) {
      val <- exists(name)
      #print(paste(name,val))
      if ( val == F ) { return( T ) }
    }
  }
  return( F )
}

isSet <- function(...) {
  names <- list(...)
  return( !(isNotSet(names)) )
}


getOrLoad <- function(rfile, varname=NULL, returnNULL=F) {
  if ( !is.null(varname) ) {
    if ( length(varname) == 1 ) { 
      if ( !isNotSet(varname) ) {
        retval <- get(varname)
        writeLines(paste(paste(" -> Returning object",varname,"size  "),objectSize(retval), sep = ": "))
        return( retval )
      }
    } else {
      if ( !isNotSet(varname) ) {
        print(varname)
        for ( var in varname ) {
          print(var)
          retval <- get(var)
          print(retval)
          writeLines(paste(paste(" -> Returning object",var,"size  "),objectSize(retval), sep = ": "))
        }
        fdata <- lapply(varname, function(x) get(x) )
        names(fdata) <- varname
        
        retval <- fdata
        print(retval)
        writeLines(paste(paste(" -> Returning object",varname,"size  "),objectSize(retval), sep = ": "))
        return( retval )
      }
    }
  }


  if ( file.exists(rfile) ) {
    writeLines(paste("Loading",rfile))
    writeLines(paste(" -> Size  ",fileSize(rfile), sep = ": "))
    lvals <- load(rfile)
    if ( length(lvals) == 0 ) {
      writeLines(paste("Nothing in the file",rfile))
      stop()
    }
    if ( length(lvals) == 1 ) {
      if ( is.null(varname) ) {
        varname <- lvals[1]
        retval <- get(varname)
        writeLines(paste(paste(" -> Returning object",varname,"size  "),objectSize(retval), sep = ": "))
        return( retval )
      } else {
        if ( varname == lvals[1] ) {
          retval <- get(varname)
          writeLines(paste(paste(" -> Returning object",varname,"size  "),objectSize(retval), sep = ": "))
          return( retval )
        } else {
          writeLines(paste(" --> Could not find",varname))
          writeLines("Found the following in the file:")
          print(lvals)
          stop()
        }
      }
    } else {
      if ( is.null(varname) ) {
        fdata <- lapply(lvals, function(x) get(x) )
        names(fdata) <- lvals
        retval <- fdata
        writeLines(paste(paste(" -> Returning object",varname,"size  "),objectSize(retval), sep = ": "))
        return( retval )
      } else {
        if ( varname %in% lvals ) {
          retval <- get(varname)
          writeLines(paste(paste(" -> Returning object",varname,"size  "),objectSize(retval), sep = ": "))
          return( retval )
        } else {
          writeLines(paste(" --> Could not find",varname))
          writeLines("Found the following in the file:")
          print(lvals)
          stop()
        }
      }
    }
  } else {
    if ( !returnNULL ) {
      writeLines(paste("Could not find",rfile))
      writeLines("You need to rerun analysis.R")
      stop()
    }
  }
  
  return( NULL )
}




getColumns <- function(jfile) {
  if ( length(jfile) == 0 ) { 
    writeLines(paste("No java file with pattern",pattern))
    return(NULL)
  }
  writeLines(paste("Getting column data from",jfile))
  lines <- readLines(jfile)
  keep <- lines[grep("setters.put", lines)]
  keep <- gsub("setters.put", "", gsub("new FieldSetterCommand()", "", keep))
  columns <- gsub("[^[:alnum:] ]", "", gsub(" ", "", keep))
  writeLines(paste("Found",length(columns),"columns"))
  return( columns )
}


readFWFData <- function(pattern, datadir="~/medicaid/data/scoop/") {
  writeLines(paste("readFWFData(",pattern,")",sep=""))
  file  <- Sys.glob(paste(paste(datadir,"*",sep="/"), pattern, "*/part*", sep = ""))
  if ( length(file) == 0 ) { 
    writeLines(paste("No file with pattern",pattern))
    return(NULL)
  }
  jfile <- Sys.glob(paste(paste(datadir,"*",sep="/"), pattern, "*.java", sep = ""))
  columns <- getColumns(jfile)
  n1line <- readLines(file, n=1)
  if ( pattern == "DEV_DIS" | pattern == "ACT_CNCR" ) { n1line <- readLines(file, n=2)[2] }
  pos <- gregexpr(',', n1line)[[1]]
  widths <- c(pos[1], diff(pos), nchar(n1line))
  pdata <- read.fwf(file, header = F, widths = widths)
  pdata <- apply(pdata, 2, function(Name) gsub(pattern = " ", "", ifelse(substring(Name, nchar(Name)) == ",", substring(Name, 1, nchar(Name)-1), Name)))
  if ( ncol(pdata) == length(columns) ) {
    colnames(pdata) <- columns
    writeLines(paste(" ---> Got it",paste(dim(pdata), collapse = " x"), sep = ": "))
    return( pdata )
  } else {
    writeLines(paste("Dim ->",ncol(pdata)))
    writeLines(paste("Len ->",length(columns)))
    print(colnames(pdata))
    print(columns)
    print(head(pdata))
    stop()
  }
}


readData <- function(pattern, fixed=F, datadir="~/medicaid/data/scoop/") {
  writeLines(paste("readData(",pattern,",",fixed,")",sep=""))
  library(data.table)
  writeLines("\n\n\n")
  writeLines(paste("readData ---->",pattern))
  if ( fixed ) { file  <- Sys.glob(paste(datadir, pattern, "/part*", sep = "")) }
  else { file  <- Sys.glob(paste(paste(datadir,"*", sep="/"), pattern, "*/part*", sep = "")) }
  if ( length(file) == 0 ) { 
    writeLines(paste("No file with pattern",pattern))
    return(NULL)
  }
  if ( fixed ) { jfile <- Sys.glob(paste(datadir, pattern, ".java", sep = "")) }
  else { jfile <- Sys.glob(paste(paste(datadir,"*",sep="/"), pattern, "*.java", sep = "")) }
  writeLines(paste("Java:",jfile))
  writeLines(paste("File:",file))
  columns <- getColumns(jfile)
  pdata <- fread(file, sep=",", data.table = F)
  if ( pattern == "GEO" ) { pdata <- pdata[,-10] }
  if ( ncol(pdata) == length(columns) ) {
    colnames(pdata) <- columns
    writeLines(paste(" ---> Got it",paste(dim(pdata), collapse = " x"), sep = ": "))
    return( pdata )
  } else {
    writeLines(paste("Dim ->",ncol(pdata)))
    writeLines(paste("Len ->",length(columns)))
    print(colnames(pdata))
    print(columns)
    print(head(pdata))
    stop()
  }
  #lname <- gsub(pattern = "_rand10k", replacement = "", unlist(strsplit(basename(file), split = "\\."))[1])
}

writeData <- function() {
  source("~/mva/formatData.R")
  library(data.table)
  cdata <- list()
  for ( file in list.files("~/medicaid/data", pattern = '*.txt', full.names = T) ) {
    lname <- gsub(pattern = "_rand10k", replacement = "", unlist(strsplit(basename(file), split = "\\."))[1])
    lname <- gsub(pattern = "_rand100k", replacement = "", lname)
    lname <- gsub(pattern = "_rand1k", replacement = "", lname)
    lname <- gsub(pattern = "_rand1M", replacement = "", lname)
    writeLines(file)
    cdata[[lname]] <- fread(file, sep="\t", data.table = F)
  }
  
  #cdata[["medicaid_members"]] <- merge(cdata[["medicaid_members"]], cdata[["fncl_mbu_cf"]], by="MBU_CF_CD")
  #cdata[["medicaid_members"]] <- merge(cdata[["medicaid_members"]], cdata[["fncl_prod_cf"]], by="PROD_CF_CD")
  
  writeLines("Saving data")
  save(cdata, file="~/medicaid/tables.rData", compress = T)
}