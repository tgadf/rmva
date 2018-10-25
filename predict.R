

predictGBM <- function(fit, x) {
  predProb  <- predict(fit, x, type="response", n.trees = fit$n.trees)
  if ( !(is.null(predProb)) ) {
    predClass <- apply(predProb, 1, function(x) which(x == max(x)))
    classes   <- colnames(predProb)
    return( classes[predClass] )
  } else {
    return( NULL )
  }
}

predictClassByReg <- function(fit, x) {
  return( round(predict(fit, x)) )
}

predictGLM <- function(fit, x, responseName) {
  predClass <- as.factor(predict(fit, x, type="response") > 0.5)
  levels(predClass) <- c(paste("Not", responseName, sep = ""), responseName)
  predClass <- makeYfac(predClass, responseName)
  return( predClass )
}

predictXGboost <- function(fit, x, responseName) {
  predClass <- as.numeric(predict(fit, x) > 0.5)
  return( predClass )
}

predictTest <- function(method, fit, testX, responseName) {
  if ( method == "xgboost" ) {
    predProb <- predict(fit, testX)
    predClass <- predictXGboost(fit, testX, responseName)
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "nnet" ) {
    predProb  <- as.data.frame(predict(fit, testX, type="raw"))
    colnames(predProb) <- responseName
    predProb  <- predProb[,responseName]
    predClass <- makeYfac(predict(fit, testX, type="class"), responseName)
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "svm" ) {
    pred     <- predict(fit, testX, decision.values = TRUE, probability = TRUE)
    predProb <- as.data.frame(attr(pred, "probabilities"))[,responseName]
    predClass <- predict(fit, testX)
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "randomForest" ) {
    predProb  <- as.data.frame(predict(fit, testX, type="prob"))[,responseName]
    predClass <- predict(fit, testX, type="class")
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "rpart" ) {
    predProb  <- as.data.frame(predict(fit, testX, type="prob"))[,responseName]
    predClass <- predict(fit, testX, type="class")
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method %in% c("ctree", "J48", "gbm", "C5.0", "nb", "avNNet", "lda", "qda") ) {
    predProb  <- as.data.frame(predict(fit, testX, type="prob"))[,responseName]
    predClass <- predict(fit, testX, type="raw")
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "ldaX" | method == "qdaX" ) {
    pred <- predict(fit, testX)
    predClass <- pred[["class"]]
    predProb <- pred[["posterior"]]
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "gbmX" ) {
    predProb  <- predict(fit, testX, type="response", n.trees = fit$n.trees)
    predClass <- predictGBM(fit, testX)
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "bagging" ) {
    predProb  <- as.data.frame(predict(fit, testX, type="prob"))[,responseName]
    predClass <- makeYfac(predict(fit, testX, type="class"), responseName = responseName)
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "bartMachine" ) {
    predProb  <- 1-predict(fit, testX, type="prob")
    predClass <- predict(fit, testX, type="class")
    #predClass <- predictClassByReg(fit, testX)
    return( list("prob"=predProb, "class"=predClass) )
  } else if ( method == "glm" | method == "glm-aic" | method == "glm-bic" | method == "lasso2" ) {
    predProb  <- predict(fit, testX, type="response")
    predClass <- predictGLM(fit, testX, responseName)
    #predClass <- as.factor(as.numeric(predClass))
    return( list("prob"=predProb, "class"=predClass) )
  }
  
  print(paste("No predict method for", method))  
  return( list(NA,NA) )
}