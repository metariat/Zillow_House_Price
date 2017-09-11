library(catboost)
library(Metrics) #for mae
library(caret) #for hyperparameter tuning
library(data.table)
gc()

#---------------------------------------------------------------#

#            Data Loading

#---------------------------------------------------------------#

source("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Code/0_1_Data_Loading_For_Modelling_By_Cities.R")



#-------------------------------------------------------------------#

#                   Defining training & testing set

#-------------------------------------------------------------------#

#----------------------------------------------------#
#----------------------------------------------------#

#             Deviding into counties

#----------------------------------------------------#
#----------------------------------------------------#
train = train[abs(train$logerror) < 0.2, ]
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
y.train.6059 = train.6059$logerror
x.train.6059 = subset(train.6059, select= -c(logerror, parcelid))

y.train.6111 = train.6111$logerror
x.train.6111 = subset(train.6111, select= -c(logerror, parcelid))

y.train.other = train.other$logerror
x.train.other = subset(train.other, select= -c(logerror, parcelid))

# Testing
y.test.6059 = test.6059$logerror
x.test.6059 = subset(test.6059, select= -c(logerror, parcelid))

y.test.6111 = test.6111$logerror
x.test.6111 = subset(test.6111, select= -c(logerror, parcelid))

y.test.other = test.other$logerror
x.test.other = subset(test.other, select= -c(logerror, parcelid))





#-------------------------------------------------------------------#

#                 Run catboost

#-------------------------------------------------------------------#

# Enable caret to use MAE as eval metric
maeSummary <- function (train,
                        lev = NULL,
                        model = NULL) {
  out <- mae(train$obs, train$pred)  
  names(out) <- "MAE"
  out
}

control <- trainControl(method = "cv",
                        number = 5,
                        verboseIter=TRUE,
                        summaryFunction = maeSummary)

grid <- expand.grid(depth = c(5),
                    learning_rate = c(0.04),
                    iterations = c(1000),
                    l2_leaf_reg = c(1e-3),
                    rsm = c(0.4),
                    border_count = c(64))



cb.6059 <- train(y          = y.train.6059,
                 x          = x.train.6059, 
                 preProcess = NULL,
                 method     = catboost.caret, 
                 metric     = "MAE", 
                 maximize   = FALSE, 
                 tuneGrid   = grid, 
                 trControl  = control)

cb.6111 <- train(y          = y.train.6111,
                 x          = x.train.6111, 
                 preProcess = NULL,
                 method     = catboost.caret, 
                 metric     = "MAE", 
                 maximize   = FALSE, 
                 tuneGrid   = grid, 
                 trControl  = control)

cb.other <- train(y          = y.train.other,
                 x          = x.train.other, 
                 preProcess = NULL,
                 method     = catboost.caret, 
                 metric     = "MAE", 
                 maximize   = FALSE, 
                 tuneGrid   = grid, 
                 trControl  = control)


#Model information
print(cb.6059)
#0.03609344

print(cb.6111)
#0.03999713

print(cb.other)
#0.04488684



#----------------------------------------------------------------------#

#                       Prediction                                     #

#----------------------------------------------------------------------#

#Prediction
gc()
cat.pred.6059 <- predict(cb.6059, x.test.6059)
cat.pred.6111 <- predict(cb.6111, x.test.6111)
cat.pred.other <- predict(cb.other, x.test.other)


cat.pred = c(cat.pred.6059, cat.pred.6111, cat.pred.other)
obs = c(y.test.6059, y.test.6111, y.test.other)

mean(abs(cat.pred - obs))
