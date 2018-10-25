### Full Tuning Script ###

library(caret)
library(e1071)
library(doMC)
registerDoMC(cores=4)
setwd("~/mva")
writeLines("source(results.R)")
source("results.R")


#################################
## Set data type
dtype="Abelone"
dtype="iris"
dtype="Adult"
dtype="housing"
dtype="ad"
dtype="census"
dtype="miniboone"
dtype="susy"
dtype="medicaid"
#dtype="member"
#dtype="um"
#dtype="claims"
source("formatData.R")
formattedData <- formatData(dtype)
formulas      <- formattedData[["formulas"]]
trainingdata  <- formattedData[["trainingData"]]
testingdata   <- formattedData[["testingData"]]
responseNames <- formattedData[["responseNames"]]
problemType   <- formattedData[["problemType"]]
#################################


#################################
## Plot data
#plotData(trainingdata, testingdata, "~/mva/data/plots.pdf")
#################################


#################################
## Set models
models <- c("xgboost", "nnet", "svm", "randomForest", "rpart", "ctree", "J48", "gbm", 
            "C5.0", "bagging", "bart", "nb", "avNNet","lda", "glm", "glmaic", "glmbic")
trainModels <- list()
trainModels <- sapply(models, function(x) trainModels[[x]] <- F)
#################################


#################################
## Set other vars
drawProbs <- F
drawMultiProbs <- F
tuneC = tune.control(sampling = "cross", cross = 2)
trainC <- trainControl(method = "cv", number = 2)
#################################


writeLines("source(train.R)")
source("train.R")


retval <- list()
if ( F ) {
#retval[["nnet-caret"]] <- runMVA("nnet", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)
#retval[["nnet"]] <- runMVA("nnet", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["xgboost"]] <- runMVA("xgboost", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["xgboost-caret"]] <- runMVA("xgboost", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)
  
retval[["rf"]] <- runMVA("rf", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["rf-caret"]] <- runMVA("rf", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)
  
retval[["gbm"]] <- runMVA("gbm", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["gbm-caret"]] <- runMVA("gbm", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)
}
if ( T ) {
  
retval[["bagging"]] <- runMVA("bagging", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["bagging-caret"]] <- runMVA("bagging", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["svm"]] <- runMVA("svm", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["svm-caret"]] <- runMVA("svm", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["rpart"]] <- runMVA("rpart", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["rpart-caret"]] <- runMVA("rpart", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["glm"]]    <- runMVA("glm", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["glmaic"]] <- runMVA("glmaic", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["glmbic"]] <- runMVA("glmbic", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)

retval[["ctree"]] <- runMVA("ctree", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["lasso"]] <- runMVA("lasso", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)

retval[["nb"]] <- runMVA("nb", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["nb-caret"]] <- runMVA("nb", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["avnnet"]] <- runMVA("avnnet", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["lda"]] <- runMVA("lda", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)
retval[["lda-caret"]] <- runMVA("lda", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

retval[["C5.0"]] <- runMVA("C5.0", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = T)

#retval[["bart"]] <- runMVA("bart", formulas, trainingdata, testingdata, responseNames, problemType = problemType, useCaret = F)  ## To do
}
save(retval, file=paste(dtype,"retvals.rData",sep="-"))

pdfname <- paste("~/mva/data/",dtype,".pdf", sep="")
plotResults(problemType, retval, testingdata, pdfname)