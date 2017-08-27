library(data.table)



#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data reading                                      #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

train = setDT(readRDS("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train.RDS"))
test = setDT(readRDS("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/test.RDS"))


train[, buildingclasstypeid:= NULL]
train[, finishedsquarefeet13.NA.type := NULL]
train[, buildingqualitytypeid:= as.factor(as.character(buildingqualitytypeid))]

test[, buildingclasstypeid:= NULL]
test[, finishedsquarefeet13.NA.type:= NULL]
test[, buildingqualitytypeid:= as.factor(as.character(buildingqualitytypeid))]

train$is.train = 1
test$is.train = 0


data = rbind(train, test)
x.data = subset(data, select = -logerror)
x.data = model.matrix(~., x.data)
x.data = data.frame(x.data)

#-------------------------------------------------------------------#
# Model without outliers #
#-------------------------------------------------------------------#
y.train = train$logerror
x.train = x.data[x.data$is.train == 1, ]
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = x.data[x.data$is.train == 0, ]
x.test = as.matrix(x.test)

#Model

library(glmnet)
grid =10^ seq (1,-20, length =100)
lasso.mod =glmnet (x.train, y.train, alpha =1, lambda =grid)
plot(lasso.mod)


cv.out = cv.glmnet (x.train, y.train, alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))


lasso.pred = predict(lasso.mod, s = bestlam, newx=x.test)
mean(abs(lasso.pred - y.test))

#RF
library(randomForest)
mod = randomForest(x = x.train, y = y.train, ntree= 2, do.trace = T)
