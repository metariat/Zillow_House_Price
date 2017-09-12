library(data.table)
library(randomForest)
library(xgboost)
library(caret)


#Data loading
source("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Code/0_Data_Loading_For_Modelling.R")

train = train[abs(logerror) < 0.2, ]
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)


# set up the cross-validated hyper-parameter search
searchGrid <- expand.grid(
  nrounds = 1,
  max_depth = c(6, 5),
  eta = c(0.01, 0.005, 0.02),
  gamma = 0,
  colsample_bytree = c(0.5, 0.6, 0.7),
  min_child_weight = c(1, 10, 20),
  subsample = c(0.6, 0.8, 0.7, 0.5)
)


#Build a xgb.DMatrix object
DMMatrixTrain <- xgb.DMatrix(data = x.train, label = y.train)

#Tuning 
maeErrorsHyperparameters <- apply(searchGrid, 1, function(parameterList){

    #Extract Parameters to test
  currentSubsampleRate  <- parameterList[["subsample"]]
  currentColsampleRate  <- parameterList[["colsample_bytree"]]
  currentMaxDepth       = parameterList[["max_depth"]]
  currentEta            = parameterList[["eta"]]
  currentMinChildWeight = parameterList[["min_child_weight"]]
  currentRounds         = parameterList[['nrounds']]

  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = currentRounds, nfold = 5, showsd = TRUE, 
                           metrics = "mae", verbose = TRUE, "eval_metric" = "mae",
                           "objective" = "reg:linear", "max.depth" = currentMaxDepth, "eta" = currentEta,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  #CV mae
  cv.mae = xgboostModelCV$evaluation_log[which.min(xgboostModelCV$evaluation_log$test_mae_mean)]
  
  #retrain the model
  round  = xgboostModelCV$evaluation_log[which.min(xgboostModelCV$evaluation_log$test_mae_mean)]$iter
  model = xgb.train(data =  DMMatrixTrain, nrounds = round, showsd = TRUE, 
                    metrics = "mae", verbose = TRUE, "eval_metric" = "mae",
                    "objective" = "reg:linear", "max.depth" = currentMaxDepth, "eta" = currentEta,                               
                    "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  #test mae
  xgb.pred = predict(model, new = x.test)
  test.mae = mean(abs(xgb.pred - y.test))
  
  sprintf("CV: %s, test: %s, eta: %s, round: %s", cv.mae, test.mae, currentEta, round)
  
  return(c(cv.mae, test.mae, currentEta, currentMaxDepth, currentMinChildWeight, currentSubsampleRate, currentColsampleRate))
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