
  rfile <- file.path(basedatadir, paste(dtype,"TrainingInputs.rData",sep="-"))
  writeLines(paste("Saving training data to",rfile))
  trainingData <- finalretval[["trainingData"]]
  save(trainingData, file = rfile, compress=T)
  
  rfile <- file.path(basedatadir, paste(dtype,"TestingInputs.rData",sep="-"))
  writeLines(paste("Saving testing data to",rfile))
  testingData <- finalretval[["testingData"]]
  save(testingData, file = rfile, compress=T)
  
  rfile <- file.path(basedatadir, paste(dtype,"TruthInputs.rData",sep="-"))
  writeLines(paste("Saving truth data to",rfile))
  truthData <- finalretval[["truthData"]]
  save(truthData, file = rfile, compress=T)
  
  rfile <- file.path(basedatadir, paste(dtype,"GeneralInputs.rData",sep="-"))
  writeLines(paste("Saving general data to",rfile))
  formulas <- finalretval[["formulas"]]
  responseNames <- finalretval[["responseNames"]]
  problemType <-   finalretval[["problemType"]]
  save(formulas, responseNames, problemType, file = rfile, compress=T)
  
  rfile <- file.path(basedatadir, paste(dtype,"RiskResults.rData",sep="-"))
  writeLines(paste("Saving risk scores to",rfile))
  save(risk56model, risk18model, acgmodel, file = rfile, compress=T)
