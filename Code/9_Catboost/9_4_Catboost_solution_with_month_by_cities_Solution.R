library(catboost)
library(Metrics) #for mae
library(caret) #for hyperparameter tuning
library(data.table)
gc()

#---------------------------------------------------------------#

#            Data Loading

#---------------------------------------------------------------#

source("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Code/0_1_Data_Loading_For_Modelling_By_Cities.R")


#----------------------------------------------------#
#----------------------------------------------------#

#             Deviding into counties

#----------------------------------------------------#
#----------------------------------------------------#

#Capping
train = train[abs(train$logerror) < 0.2, ]
test = test[abs(test$logerror) < 0.2, ]

#Training data aggregation
train = rbind(train, test)

#Devide into cities
train.6059 = train[train$fips6059 == 1, ]
train.6111 = train[train$fips6111 == 1, ]
train.other = train

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

gc()

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

#Training mae
cat.pred.6059 <- predict(cb.6059, x.train.6059)
cat.pred.6111 <- predict(cb.6059, x.train.6111)
cat.pred.other <- predict(cb.6059, x.train.other)

cat.pred = c(cat.pred.6059, cat.pred.6111, cat.pred.other)
obs = c(y.train.6059, y.train.6111, y.train.other)
mean(abs(cat.pred - obs))

#----------------------------------------------------------------------#

#                       Prediction                                     #

#----------------------------------------------------------------------#

#Prediction 6059
gc()
properties.6059 = properties[properties$fips6059 == 1, ]
parcelid.6059 = properties.6059$parcelid
properties.6059 = subset(properties.6059, select= -c(parcelid))
gc()
properties.6059[, month := 10]
cat.pred.6059.10 <- 0.97*predict(cb.6059, properties.6059) + 0.03*0.11
gc()
properties.6059[, month := 11]
cat.pred.6059.11 <- 0.97*predict(cb.6059, properties.6059) + 0.03*0.11
gc()
properties.6059[, month := 12]
cat.pred.6059.12 <- 0.97*predict(cb.6059, properties.6059) + 0.03*0.11
gc()
pred.6059 = data.frame("parcelid"  = parcelid.6059, 
                       "pred_10"   = cat.pred.6059.10,
                       "pred_11"   = cat.pred.6059.11,
                       "pred_12"   = cat.pred.6059.12)
rm(properties.6059)
gc()


#Prediction 6111
gc()
properties.6111 = properties[properties$fips6111 == 1, ]
parcelid.6111 = properties.6111$parcelid
properties.6111 = subset(properties.6111, select= -c(parcelid))
gc()
properties.6111[, month := 10]
cat.pred.6111.10 <- 0.97*predict(cb.6111, properties.6111) + 0.03*0.11
gc()
properties.6111[, month := 11]
cat.pred.6111.11 <- 0.97*predict(cb.6111, properties.6111) + 0.03*0.11
gc()
properties.6111[, month := 12]
cat.pred.6111.12 <- 0.97*predict(cb.6111, properties.6111) + 0.03*0.11
gc()
pred.6111 = data.frame("parcelid"  = parcelid.6111, 
                       "pred_10"   = cat.pred.6111.10,
                       "pred_11"   = cat.pred.6111.11,
                       "pred_12"   = cat.pred.6111.12)
rm(properties.6111)
gc()


#Prediction other
gc()
properties.other = properties[properties$fips6059 == 0 & properties$fips6111 == 0, ]
parcelid.other = properties.other$parcelid
properties.other = subset(properties.other, select= -c(parcelid))
gc()
properties.other[, month := 10]
cat.pred.other.10 <- 0.97*predict(cb.other, properties.other) + 0.03*0.11
gc()
properties.other[, month := 11]
cat.pred.other.11 <- 0.97*predict(cb.other, properties.other) + 0.03*0.11
gc()
properties.other[, month := 12]
cat.pred.other.12 <- 0.97*predict(cb.other, properties.other) + 0.03*0.11
gc()
pred.other = data.frame("parcelid"  = parcelid.other, 
                       "pred_10"   = cat.pred.other.10,
                       "pred_11"   = cat.pred.other.11,
                       "pred_12"   = cat.pred.other.12)
rm(properties.other)
gc()
  
pred.data = rbind(pred.6059, pred.6111, pred.other)

submission = subset(properties, select = parcelid)
submission = merge(submission, pred.data, by = "parcelid", all.x = T)

submission[, pred_10 := round(pred_10, 5)]
submission[, pred_11 := round(pred_11, 5)]
submission[, pred_12 := round(pred_12, 5)]


submission[, pred_10_7 := pred_10]
submission[, pred_11_7 := pred_11]
submission[, pred_12_7 := pred_12]
submission[, parcelid := as.integer(as.character(parcelid))]


setnames(submission, old = c("pred_10", "pred_11", "pred_12", "pred_10_7", "pred_11_7", "pred_12_7"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))
fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170911_Q_tuned_catboost_with_month_by_cities.csv")
