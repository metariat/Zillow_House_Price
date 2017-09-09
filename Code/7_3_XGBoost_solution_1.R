library(data.table)
library(xgboost)
path.code = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"
train = setDT(readRDS(paste0(path.code,"train.RDS")))
test = setDT(readRDS(paste0(path.code,"test.RDS")))

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train[, tract.number := NULL]
test[, tract.number := NULL]

train = train[abs(train$logerror) < 0.2, ]
test = test[abs(test$logerror) < 0.2, ]

data = rbind(train, test)
y = data$logerror
x = subset(data, select = -c(logerror, parcelid))
x = as.matrix(x)

set.seed(1024)
y_mean = mean(y)

param <- list(objective           = "reg:linear",
              eval_metric         = "mae",
              eta                 = 0.037,
              max_depth           = 5, 
              subsample           = 0.8,
              colsample_bytree    = 0.5,
              min_child_weight    = 4,
              maximize            = FALSE,
              lambda              = 0.8,
              alpha               = 0.4,
              base_score          = y_mean,
              silent              = 0)

xgb_cv <- xgb.cv(data = x,
                 label = y,
                 params = param,
                 nrounds = 700,
                 prediction = TRUE,
                 maximize = FALSE,
                 nfold = 5,
                 print_every_n = 5)

round = xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]$iter

dtrain = xgb.DMatrix(as.matrix(x),label = y)
model = xgb.train(param, data = dtrain, nrounds = round)

#training mae
xgb.pred = predict(model, new = x)
mean(abs(xgb.pred - y))



#-------------------------------------------------------#
#-------------------------------------------------------#

#                       PREDICT

#-------------------------------------------------------#
#-------------------------------------------------------#

#Read the cleaned properties data
properties = readRDS("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_v2_sparse_matrix.RDS")
parcelid = data.frame(properties)$parcelid

xgb.pred = predict(model, new = properties)
xgb.pred = round(xgb.pred, 5)

submission = data.frame("parcelid" = parcelid, "a_201610" = xgb.pred)
submission = setDT(submission)
submission[, a_201611 := a_201610]
submission[, a_201612 := a_201610]
submission[, a_201710 := a_201610]
submission[, a_201711 := a_201610]
submission[, a_201712 := a_201610]
submission[, parcelid := as.integer(as.character(parcelid))]

setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))

fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170904_Q_simple_XGB_without_outliers.csv")
