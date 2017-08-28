library(data.table)
library(xgboost)
train = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/train.RDS"))
test = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/test.RDS"))

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

y = c(y.train, y.test)
x = rbind(x.train, x.test)


set.seed(1024)

y_mean = mean(y)

param <- list(objective           = "reg:linear",
              eval_metric         = "mae",
              eta                 = 0.037,
              max_depth           = 5, 
              subsample           = 0.8,
              lambda              = 0.8,
              alpha               = 0.4,
              base_score          = y_mean,
              silent              = 1)

xgb_cv <- xgb.cv(data = x,
                 label = y,
                 params = param,
                 nrounds = 300,
                 prediction = TRUE,
                 maximize = FALSE,
                 nfold = 5,
                 print_every_n = 5)


plot(xgb_cv$evaluation_log$train_mae_mean, type = "l")
lines(xgb_cv$evaluation_log$test_mae_mean, type = "l", col = "red")

print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]) #0.05271467
round = xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]$iter

dtrain = xgb.DMatrix(as.matrix(x),label= y)

model = xgb.train(param, data = dtrain, nrounds=round)
importance <- xgb.importance(colnames(dtrain), model = model)
xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative importance")
(gg <- xgb.ggplot.importance(importance, measure = "Frequency", rel_to_first = TRUE))
gg + ggplot2::ylab("Frequency")

#Read the cleaned properties data
properties = readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/properties_v2_sparse_matrix.RDS")
properties = as.matrix(properties)

xgb.pred = predict(model, new = properties)
xgb.pred = round(xgb.pred, 5)

submission = data.frame("parcelid" = data.frame(properties)$parcelid, "a_201610" = xgb.pred)
submission = setDT(submission)
submission[, a_201611 := a_201610]
submission[, a_201612 := a_201610]
submission[, a_201710 := a_201610]
submission[, a_201711 := a_201610]
submission[, a_201712 := a_201610]
submission[, parcelid := as.integer(as.character(parcelid))]

setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))

fwrite(submission, "C:/Quang/Kaggle/Zillow_House_Price_Data/submissions/20172808_Q_XGB_without_outliers.csv")
