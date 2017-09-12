library(data.table)
library(lightgbm)
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
params <- list(
  learning_rate = 0.001,
  max_depth = 5,
  max_bin = 10,
  boosting_type = 'gbdt',
  objective = 'regression',
  metric = 'MAE',
  sub_feature = 0.345,
  bagging_fraction = 0.85,
  bagging_freq = 40,
  num_leaves = 512,
  min_data = 500,
  min_hessian = 0.05,
  verbose = 0
)

dtrain <- lgb.Dataset(data = x.train, label = y.train)

model = lgb.cv(params,
             dtrain,
             50,
             nfold = 5,
             verbose = 1)

rm(properties)
gc()

#Build a xgb.DMatrix object






#Tuning 
maeErrorsHyperparameters <- apply(searchGrid, 1, function(parameterList){
  
  #Extract Parameters to test
  
  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = currentRounds, nfold = 5, showsd = TRUE, 
                           metrics = "mae", verbose = F, "eval_metric" = "mae",
                           "objective" = "reg:linear", "max.depth" = currentMaxDepth, "eta" = currentEta,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  #CV mae
  cv.mae = xgboostModelCV$evaluation_log[which.min(xgboostModelCV$evaluation_log$test_mae_mean)]$test_mae_mean
  
  #retrain the model
  round  = xgboostModelCV$evaluation_log[which.min(xgboostModelCV$evaluation_log$test_mae_mean)]$iter
  model = xgb.train(data =  DMMatrixTrain, nrounds = round, showsd = TRUE, 
                    metrics = "mae", verbose = F, "eval_metric" = "mae",
                    "objective" = "reg:linear", "max.depth" = currentMaxDepth, "eta" = currentEta,                               
                    "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  #test mae
  xgb.pred = predict(model, new = x.test)
  test.mae = mean(abs(xgb.pred - y.test))
  gc()
  
  print(paste0("CV:", cv.mae, "  test:", test.mae, 
               "  eta:", currentEta, "  round:", round))
  
  return(c(cv.mae, test.mae, currentEta, round, currentMaxDepth, currentMinChildWeight, currentSubsampleRate, currentColsampleRate))
})

maeErrorsHyperparameters
