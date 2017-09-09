library(data.table)
library(randomForest)
library(xgboost)
library(caret)


train = setDT(readRDS("Z:/Users/Q.DO/Poubelle/train.RDS"))
test = setDT(readRDS("Z:/Users/Q.DO/Poubelle/test.RDS"))

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train = train[train$logerror > -0.4 & train$logerror < 0.419, ]
test = test[test$logerror > -0.4 & test$logerror < 0.419, ]


y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)




# set up the cross-validated hyper-parameter search
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