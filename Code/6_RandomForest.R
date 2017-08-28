library(data.table)
library(randomForest)
train = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/train.RDS"))
test = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/test.RDS"))

train[, transactiondate := NULL]
test[, transactiondate := NULL]

y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)


rf <- foreach(ntree=rep(250, 2), .combine=combine, .multicombine=TRUE,
              .packages='randomForest') %dopar% {
                randomForest(x = x.train, y = y.train, ntree=ntree, importance = T, do.trace = T)
              }

varImpPlot(rf)

