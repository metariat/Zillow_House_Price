library(data.table)
library(glmnet)

#-------------------------------------------------------------------#

# Solution without outliers capping = 0.2 & transformed variables

#-------------------------------------------------------------------#


#   1. DATA Loading
#---------------------------------


path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path, "train.RDS")))
test = setDT(readRDS(paste0(path, "test.RDS")))
properties = setDT(data.frame(readRDS(paste0(path, "properties_v2_sparse_matrix.RDS"))))
parcelid = properties$parcelid

#renaming
setnames(train, old = "N.prop.typeNot Built", new = "N.prop.typeNot.Built")
setnames(test, old = "N.prop.typeNot Built", new = "N.prop.typeNot.Built")
setnames(properties, old = "X.Intercept.", new = "(Intercept)")


train[, transactiondate := NULL]
test[, transactiondate := NULL]
train[, tract.number := NULL]
test[, tract.number := NULL]
properties[, tract.number := NULL]
gc()



# 2. Transforming high skew variable
#------------------------------------

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

gc()

train = train[abs(train$logerror) < 0.2]
test = test[abs(test$logerror) < 0.2]

y = c(train$logerror, test$logerror)
x = rbind(train, test)
x = subset(x, select = -c(logerror))
x = as.matrix(x)

#model
cv.out = cv.glmnet (x = x, y = y, alpha = 1, nfolds = 7, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min #best lambda
cv.out$cvm[match(bestlam, cv.out$lambda)] #cv mae min
cv.out$cvsd[match(bestlam, cv.out$lambda)] #cv mae min sd
lasso.pred = predict(cv.out, s = bestlam, newx=x)
mean(abs(lasso.pred - y))



#Prediction
properties = as.matrix(properties)
gc()

rm(x, y, train, test)
gc()

lasso.pred1 = predict(cv.out, s = bestlam, newx = properties[1:1000000, ])
gc()
lasso.pred2 = predict(cv.out, s = bestlam, newx = properties[1000001:2000000, ])
gc()
lasso.pred3 = predict(cv.out, s = bestlam, newx = properties[2000001:nrow(properties), ])
gc()
lasso.pred = c(lasso.pred1, lasso.pred2, lasso.pred3)
lasso.pred = round(lasso.pred, 5)



# Create the submission file
#--------------------------------

submission = data.frame("parcelid" = parcelid, "201610" = lasso.pred)
setnames(submission, old = "X201610", new = "a_201610")
submission = setDT(submission)
submission[, a_201611 := a_201610]
submission[, a_201612 := a_201610]
submission[, a_201710 := a_201610]
submission[, a_201711 := a_201610]
submission[, a_201712 := a_201610]
submission[, parcelid := as.integer(as.character(parcelid))]

setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))

fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170902_Q_linear_lasso_without_outliers_cap_02_trans_var.csv")
