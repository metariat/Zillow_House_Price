library(data.table)
library(xgboost)


#-------------------------------------------------#
#                    Data loading                 #
#-------------------------------------------------#
source("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Code/0_Data_Loading_For_Modelling.R")

#add month feature
train$transactiondate = as.Date(train$transactiondate, "%Y-%m-%d")
test$transactiondate = as.Date(test$transactiondate, "%Y-%m-%d")

train[, month := month(transactiondate)]
test[, month :=month(transactiondate)]

train[, transactiondate := NULL]
test[, transactiondate := NULL]


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
              eta                 = 0.02,
              max_depth           = 6, 
              subsample           = 0.7,
              colsample_bytree    = 0.7,
              min_child_weight    = 1,
              base_score          = y_mean)

xgb_cv <- xgb.cv(data = x,
                 label = y,
                 params = param,
                 nrounds = 800,
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

#Prediction
properties[, month := 10]
properties = as.matrix(properties)
gc()
xgb.pred.10 <- 0.97*predict(model, properties) + 0.03*0.11

#11
gc()
properties = data.frame(properties)
gc()
properties$month = 11
gc()
properties = as.matrix(properties)
gc()
xgb.pred.11 <- 0.97*predict(model, properties) + 0.03*0.11

#12
gc()
properties = data.frame(properties)
gc()
properties$month = 12
gc()
properties = as.matrix(properties)
gc()
xgb.pred.12 <- 0.97*predict(model, properties) + 0.03*0.11
gc()

#round
xgb.pred.10 = round(xgb.pred.10, 5)
xgb.pred.11 = round(xgb.pred.11, 5)
xgb.pred.12 = round(xgb.pred.12, 5)

submission = data.frame("parcelid" = as.integer(as.character(data.frame(properties)$parcelid)), 
                        "a_201610" = xgb.pred.10,
                        "a_201611" = xgb.pred.11,
                        "a_201612" = xgb.pred.12,
                        "a_201710" = xgb.pred.10,
                        "a_201711" = xgb.pred.11,
                        "a_201712" = xgb.pred.12)
gc()
submission = setDT(submission)
setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))
fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170913_Q_tuned_xgboost_with_month.csv")
