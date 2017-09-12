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
xgb.grid = expand.grid(
  nrounds = 500,
  max_depth = c(6, 5),
  eta = c(0.01, 0.005, 0.02),
  gamma = 0,
  colsample_bytree = c(0.5, 0.6, 0.7),
  min_child_weight = c(1, 10, 20),
  subsample = c(0.6, 0.8, 0.7, 0.5)
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
  method = "xgbTree",
  metric = "MAE"
)
time2 = Sys.time()

#round 1:
#nrounds = 5000, max_depth = 4, eta = 0.01, gamma = 1, colsample_bytree = 1, min_child_weight = 1
#subsample = 0.6

#round 2:
#Fitting nrounds = 500, max_depth = 6, eta = 0.01, gamma = 0, colsample_bytree = 0.6, min_child_weight = 10, subsample = 0.8 on full training set
