require(mlr)
dataset   <- "iris"
mva       <- "xgboost"
target    <- "setosa"
targetcol <- "Species"

data(iris)



fdata <- iris
ldata <- setBinaryTargetClass(df = fdata, targetcol = targetcol, target = target)
targetcol <- "TARGET"
fdata <- splitTestTrainData(ldata, fracTrain = 0.4, dSample = T, debug = T)

targetType <- getTargetType(fdata[,targetcol], mva, debug = T)
fdata[,targetcol] <- formatTarget(fdata[,targetcol], target = target, targetType, debug = T)
print(fdata[,targetcol])

train      <- getTrainData(fdata)
test       <- getTestData(fdata)
truth      <- getTestTarget(fdata)



## 1) Define the task
## Specify the type of analysis (e.g. classification) and provide data and response variable
task = makeClassifTask(data = iris, target = "Species")

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrn = makeLearner("classif.lda")

n = nrow(iris)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) Fit the model
## Train the learner on the task using a random subset of the data as training set
model = mlr::train(lrn, task, subset = train.set)

## 4) Make predictions
## Predict values of the response variable for new observations by the trained model
## using the other part of the data as test set
pred = predict(model, task = task, subset = test.set)

## 5) Evaluate the learner
## Calculate the mean misclassification error and accuracy
performance(pred, measures = list(mmce, acc))
#> mmce  acc 
#> 0.04 0.96


listLearners("classif")[c("class","package")]