
library(data.table)
library(glmnet)
library(moments) #skewness calculation


#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data reading                                      #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

path = "C:/Quang/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path,"train.RDS")))
test = setDT(readRDS(paste0(path,"test.RDS")))
train = train[abs(train$logerror) < 0.4, ]
tract = train$tract.number

train[, transactiondate := NULL]
train[, tract.number := NULL]

#-------------------------------------------------------------------#

# Model with outliers #

#-------------------------------------------------------------------#
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

#Model
cv.out = cv.glmnet (x.train, y.train, alpha = 1, nfolds = 10, type.measure = "mae", parallel = T)
plot(cv.out)
bestlam = cv.out$lambda.min #best lambda
cv.out$cvm[match(bestlam, cv.out$lambda)] #cv mae min
cv.out$cvsd[match(bestlam, cv.out$lambda)] #cv mae min sd

#train error
lasso.pred = predict(cv.out, s = bestlam, newx=x.train)
mean(abs(lasso.pred - y.train))


data = data.frame("insee" = tract, 'X' = train$longitude, 'Y' = train$latitude, 'Weight' = 1, 'obs' = y.train, 'pred' = lasso.pred)
data$random = sample.int(10, nrow(data), replace = T)
fwrite(data, "Z:/Users/Q.DO/Poubelle/XGB_residuals.csv", sep = ";")

