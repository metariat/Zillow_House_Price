library(data.table)
library(glmnet)
library(moments) #skewness calculation


#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data reading                                      #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path,"train.RDS")))
test = setDT(readRDS(paste0(path,"test.RDS")))

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train[, tract.number := NULL]
test[, tract.number := NULL]



#-------------------------------------------------------------------#

# Model with outliers #

#-------------------------------------------------------------------#
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)

#Model
cv.out = cv.glmnet (x.train, y.train, alpha = 1, nfolds = 10, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min #best lambda
cv.out$cvm[match(bestlam, cv.out$lambda)] #cv mae min
cv.out$cvsd[match(bestlam, cv.out$lambda)] #cv mae min sd

#train error
lasso.pred = predict(cv.out, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))

#test error
lasso.pred = predict(cv.out, s = bestlam, newx=x.test)
mean(abs(lasso.pred - y.test))





#-------------------------------------------------------------------#

# Model without outliers #

#-------------------------------------------------------------------#
train = train[abs(logerror) < 0.2, ]
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)


#Model
cv.out = cv.glmnet (x.train, y.train, alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min #best lambda
cv.out$cvm[match(bestlam, cv.out$lambda)] #cv mae min
cv.out$cvsd[match(bestlam, cv.out$lambda)] #cv mae min sd
#training mae
lasso.pred = predict(cv.out, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))

#testing mae
lasso.pred = predict(cv.out, s = bestlam, newx=x.test)
mean(abs(lasso.pred - y.test))








#-------------------------------------------------------------------#

# Model without outliers and with transformed variables

#-------------------------------------------------------------------#



path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path,"train.RDS")))
test = setDT(readRDS(paste0(path,"test.RDS")))

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train[, tract.number := NULL]
test[, tract.number := NULL]

skew.var = c("basementsqft",
             "bathroomcnt",
             "calculatedbathnbr",
             "finishedfloor1squarefeet",
             "calculatedfinishedsquarefeet",
             "finishedsquarefeet12",
             "finishedsquarefeet13",
             "finishedsquarefeet15",
             "finishedsquarefeet50",
             "finishedsquarefeet6",
             "fireplacecnt",
             "fullbathcnt",
             "garagecarcnt",
             "lotsizesquarefeet",
             "poolsizesum",
             "roomcnt",
             "unitcnt",
             "yardbuildingsqft17",
             "yardbuildingsqft26",
             "structuretaxvaluedollarcnt",
             "taxvaluedollarcnt",
             "landtaxvaluedollarcnt",
             "taxamount",
             "N.living.area.error",
             "N.living.area.prop",
             "N.LivingAreaProp2",
             "N.ExtraSpace",
             "N.TotalRooms",
             "N.ValueProp",
             "N.value.ratio",
             "N.tax.score",
             "N.life.tax",
             "N.zip.count",
             "N.city.count",
             "N.tract.count",
             "water.distance")

for (i in skew.var){
  min.va = min(min(train[, get(i)]), min(test[, get(i)]), 0)
  train[, eval(i) := log(get(i) - min.va + 1)]
  test[, eval(i) := log(get(i) - min.va + 1)]
}


train = train[abs(logerror) < 0.2, ]
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))
x.test = as.matrix(x.test)


#Model
cv.out = cv.glmnet (x.train, y.train, alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min #best lambda
cv.out$cvm[match(bestlam, cv.out$lambda)] #cv mae min
cv.out$cvsd[match(bestlam, cv.out$lambda)] #cv mae min sd
#training mae
lasso.pred = predict(cv.out, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))

#testing mae
lasso.pred = predict(cv.out, s = bestlam, newx=x.test)
mean(abs(lasso.pred - y.test))
