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
#                 Data formating                                  #
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



#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                       Outlier definition & data preparation

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#Definition of outliers
train[, is.outlier := ifelse(abs(logerror) > 0.2, 1, 0)]
train[, is.outlier := as.factor(is.outlier)]
train[, logerror := NULL]


#In which counties there are alot of outliers?
train[fips6059 == 1, table(is.outlier) / length(is.outlier) * 100]
train[fips6111 == 1, table(is.outlier) / length(is.outlier) * 100]
train[fips6059 == 0 & fips6111 == 0, table(is.outlier) / length(is.outlier) * 100]


#train - test separation
train.index = sample(1:nrow(train), size = round(0.5 * nrow(train)), replace = FALSE)
training = train[train.index ,]
testing = train[-train.index ,]

y.train = training$is.outlier
x.train = subset(training, select = -c(is.outlier, parcelid))
x.train = as.matrix(x.train)

y.test = testing$is.outlier
x.test = subset(testing, select = -c(is.outlier, parcelid))
x.test = as.matrix(x.test)

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#       Modelling

#----------------------------------------------------------------#
#----------------------------------------------------------------#

cv.out = cv.glmnet (x = x.train, y = as.factor(y.train), family = "binomial", 
                    alpha = 1, nfolds = 5, type.measure = "auc", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min #best lambda
cv.out$cvm[match(bestlam, cv.out$lambda)] #cv mae min
cv.out$cvsd[match(bestlam, cv.out$lambda)] #cv mae min sd
lasso.pred = predict(cv.out, s = bestlam, newx = x.train, type = "response")
mean(abs(lasso.pred - as.numeric(as.character(y.train))))

x = subset(train, select = -c(is.outlier, parcelid))
x2 = subset(test, select = -c(logerror, parcelid))
x = as.matrix(x)
x2 = as.matrix(x2)
prob.outlier.train = predict(cv.out, s = bestlam, newx = x, type = "response")
prob.outlier.test = predict(cv.out, s = bestlam, newx = x2, type = "response")
train.outlier.prob = data.frame("parcelid" = train$parcelid, "prob" = prob.outlier.train)
test.outlier.prob = data.frame("parcelid" = test$parcelid, "prob" = prob.outlier.test)

fwrite(train.outlier.prob, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/train_outlier_prob.csv")
fwrite(test.outlier.prob, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/test_outlier_prob.csv")