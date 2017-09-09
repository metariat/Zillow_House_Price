library(data.table)
library(randomForest)
library(xgboost)
library(caret)

path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path, "train.RDS")))
test = setDT(readRDS(paste0(path, "test.RDS")))

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train[, tract.number := NULL]
test[, tract.number := NULL]

train = train[abs(train$logerror) < 0.2, ]
y.train = train$logerror
x.train = subset(train, select= -c(parcelid, logerror))
x.train = as.matrix(x.train)
# set up the cross-validated hyper-parameter search

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1), 
                                colsample_bytree = c(0.6, 0.8, 1))


#Build a xgb.DMatrix object
DMMatrixTrain <- xgb.DMatrix(data = x.train, label = y.train)

#Tuning 

ntrees <- 100

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){

    #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]

  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                           metrics = "mae", verbose = TRUE, "eval_metric" = "mae",
                           "objective" = "reg:linear", "max.depth" = 5, "eta" = 2/ntrees,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
  #Save mae of the last iteration
  mae <- tail(xvalidationScores$test_mae_mean, 1)
  
  return(c(mae, currentSubsampleRate, currentColsampleRate))
  
})

rmseErrorsHyperparameters

xgb.grid = expand.grid(
  nrounds = 5000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8),
  gamma = c(1,5),
  colsample_bytree = c(0.4, 0.6, 1),
  min_child_weight = c(1, 10, 20),
  subsample = c(1, 0.6)
)


# pack the training control parameters
xgb.trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  #classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  allowParallel = TRUE
)

time1 = Sys.time()
# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb.train = train(
  x = x.train,
  y = y.train,
  trControl = xgb.trcontrol,
  tuneGrid = xgb.grid,
  method = "xgbTree"
)
time2 = Sys.time()