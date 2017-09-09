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
properties = setDT(data.frame(readRDS(paste0(path, "properties_v2_sparse_matrix.RDS"))))

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train[, tract.number := NULL]
test[, tract.number := NULL]




#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data formatting                                     #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
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

gc()
for (i in skew.var){
  min.va = min(min(properties[, get(i)]), 0)
  train[, eval(i) := log(get(i) - min.va + 1)]
  test[, eval(i) := log(get(i) - min.va + 1)]
  properties[, eval(i) := log(get(i) - min.va + 1)]
}
rm(properties)
gc()




#----------------------------------------------------#
#----------------------------------------------------#

#             Deviding into counties

#----------------------------------------------------#
#----------------------------------------------------#
train.6059 = train[train$fips6059 == 1, ]
train.6111 = train[train$fips6111 == 1, ]
train.other = train[train$fips6059 == 0 & train$fips6111 == 0, ]

test.6059 = test[test$fips6059 == 1, ]
test.6111 = test[test$fips6111 == 1, ]
test.other = test[test$fips6059 == 0 & test$fips6111 == 0, ]






#----------------------------------------------------#
#----------------------------------------------------#

#             Data prep for training

#----------------------------------------------------#
#----------------------------------------------------#


#    Training 
train.6059 = train.6059[abs(train.6059$logerror) < 0.2, ]
y.train.6059 = train.6059$logerror
x.train.6059 = subset(train.6059, select= -c(logerror, parcelid))
x.train.6059 = as.matrix(x.train.6059)

train.6111 = train.6111[abs(train.6111$logerror) < 0.2, ]
y.train.6111 = train.6111$logerror
x.train.6111 = subset(train.6111, select= -c(logerror, parcelid))
x.train.6111 = as.matrix(x.train.6111)

train.other = train.other[abs(train.other$logerror) < 0.2, ]
y.train.other = train.other$logerror
x.train.other = subset(train.other, select= -c(logerror, parcelid))
x.train.other = as.matrix(x.train.other)




# Testing
y.test.6059 = test.6059$logerror
x.test.6059 = subset(test.6059, select= -c(logerror, parcelid))
x.test.6059 = as.matrix(x.test.6059)

y.test.6111 = test.6111$logerror
x.test.6111 = subset(test.6111, select= -c(logerror, parcelid))
x.test.6111 = as.matrix(x.test.6111)

y.test.other = test.other$logerror
x.test.other = subset(test.other, select= -c(logerror, parcelid))
x.test.other = as.matrix(x.test.other)



#----------------------------------------------------#
#----------------------------------------------------#

#             Model training

#----------------------------------------------------#
#----------------------------------------------------#
cv.out.6059 = cv.glmnet (x.train.6059, y.train.6059, 
                         alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)
cv.out.6111 = cv.glmnet (x.train.6111, y.train.6111, 
                         alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)
cv.out.other = cv.glmnet (x.train.other, y.train.other, 
                         alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)

#plot
par(mfrow = c(1,3))
plot(cv.out.6059)
plot(cv.out.6111)
plot(cv.out.other)

bestlam.6059 = cv.out.6059$lambda.min #best lambda
bestlam.6111 = cv.out.6111$lambda.min #best lambda
bestlam.other = cv.out.other$lambda.min #best lambda


#Training mae
lasso.pred.6059 = predict(cv.out.6059, s = bestlam.6059, newx = x.train.6059)
lasso.pred.6111 = predict(cv.out.6111, s = bestlam.6111, newx = x.train.6111)
lasso.pred.other = predict(cv.out.other, s = bestlam.other, newx = x.train.other)

lasso.pred = c(lasso.pred.6059, lasso.pred.6111, lasso.pred.other)
y.train = c(y.train.6059, y.train.6111, y.train.other)
mean(abs(lasso.pred - y.train))



#Testing mae
lasso.pred.6059 = predict(cv.out.6059, s = bestlam.6059, newx = x.test.6059)
lasso.pred.6111 = predict(cv.out.6111, s = bestlam.6111, newx = x.test.6111)
lasso.pred.other = predict(cv.out.other, s = bestlam.other, newx = x.test.other)

lasso.pred = c(lasso.pred.6059, lasso.pred.6111, lasso.pred.other)
y.test = c(y.test.6059, y.test.6111, y.test.other)
mean(abs(lasso.pred - y.test))

