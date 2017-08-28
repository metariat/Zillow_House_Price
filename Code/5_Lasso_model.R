library(data.table)



#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data reading                                      #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

train = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/train.RDS"))
test = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/test.RDS"))

train[, transactiondate := NULL]
test[, transactiondate := NULL]

#-------------------------------------------------------------------#
# Model without outliers #
#-------------------------------------------------------------------#
y.train = train$logerror
x.train = subset(train, select= -c(logerror))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror))
x.test = as.matrix(x.test)

y = c(y.train, y.test)
x = rbind(x.train, x.test)
x = as.matrix(x)

#Model

library(glmnet)
cv.out = cv.glmnet (x, y, alpha = 1, nfolds = 10, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(cv.out, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))


#Read the cleaned properties data
properties = readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/properties_v2_sparse_matrix.RDS")
properties = as.matrix(properties)

dim(properties)

lasso.pred = predict(cv.out, s = bestlam, newx=properties)
lasso.pred = round(lasso.pred, 5)

submission = data.frame("parcelid" = data.frame(properties)$parcelid, "201610" = lasso.pred)
setnames(submission, old = "X1", new = "a_201610")
submission = setDT(submission)
submission[, a_201611 := a_201610]
submission[, a_201612 := a_201610]
submission[, a_201710 := a_201610]
submission[, a_201711 := a_201610]
submission[, a_201712 := a_201610]
submission[, parcelid := as.integer(as.character(parcelid))]

setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
                     new = c("201610", "201611", "201612", "201710", "201711", "201712"))

fwrite(submission, "C:/Quang/Kaggle/Zillow_House_Price_Data/submissions/20172808_Q_linear_lasso_with_outliers.csv")















#-------------------------------------------------------------------#
# Model without outliers #
#-------------------------------------------------------------------#
train = train[abs(logerror) < 1.5, ]
y.train = train$logerror
x.train = subset(train, select= -c(logerror))
x.train = as.matrix(x.train)

y.test = test$logerror
x.test = subset(test, select= -c(logerror))
x.test = as.matrix(x.test)


#Model

library(glmnet)
cv.out = cv.glmnet (x.train, y.train, alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(cv.out, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))


lasso.pred = predict(cv.out, s = bestlam, newx=x.test)
mean(abs(lasso.pred - y.test))

y = c(y.train, y.test)
x = as.matrix(rbind(x.train, x.test))
cv.out = cv.glmnet (x = x, y = y, alpha = 1, nfolds = 5, type.measure = "mae", parallel = T)





#Read the cleaned properties data
properties = readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/properties_v2_sparse_matrix.RDS")
properties = as.matrix(properties)

dim(properties)

lasso.pred = predict(cv.out, newx = properties)
lasso.pred = round(lasso.pred, 5)

submission = data.frame("parcelid" = data.frame(properties)$parcelid, "201610" = lasso.pred)
setnames(submission, old = "X1", new = "a_201610")
submission = setDT(submission)
submission[, a_201611 := a_201610]
submission[, a_201612 := a_201610]
submission[, a_201710 := a_201610]
submission[, a_201711 := a_201610]
submission[, a_201712 := a_201610]
submission[, parcelid := as.integer(as.character(parcelid))]

setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))

fwrite(submission, "C:/Quang/Kaggle/Zillow_House_Price_Data/submissions/20172808_Q_linear_lasso_without_outliers_cap_1_point_5.csv")