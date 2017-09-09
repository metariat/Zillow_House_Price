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
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)

set.seed(1024)
y_mean = mean(y.train)

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

xgb_cv <- xgb.cv(data = x.train,
                 label = y.train,
                 params = param,
                 nrounds = 1000,
                 prediction = TRUE,
                 maximize = FALSE,
                 nfold = 5,
                 print_every_n = 5)


plot(xgb_cv$evaluation_log$train_mae_mean, type = "l")
lines(xgb_cv$evaluation_log$test_mae_mean, type = "l", col = "red")

print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]) #0.0402418
round = xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]$iter

dtrain = xgb.DMatrix(as.matrix(x.train),label = y.train)

model = xgb.train(param, data = dtrain, nrounds = round)
importance <- xgb.importance(colnames(dtrain), model = model)
xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative importance")


#training mae
xgb.pred = predict(model, new = x.train)
mean(abs(xgb.pred - y.train))

#testing mae
xgb.pred = predict(model, new = x.test)
mean(abs(xgb.pred - y.test))








