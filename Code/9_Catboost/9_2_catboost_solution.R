library(catboost)
library(Metrics) #for mae
library(caret) #for hyperparameter tuning


#---------------------------------------------------------------#

#            Data Loading

#---------------------------------------------------------------#
source("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Code/0_Data_Loading_For_Modelling.R")


#-------------------------------------------------------------------#

#                   Defining training & testing set

#-------------------------------------------------------------------#
train = train[abs(train$logerror) < 0.2, ]
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))

test = test[abs(test$logerror) < 0.2, ]
y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))

y = c(y.train, y.test)
x = rbind(x.train, x.test)

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
                    learning_rate = c(0.06),
                    iterations = c(600),
                    l2_leaf_reg = c(1e-3),
                    rsm = c(0.6),
                    border_count = c(64))

cb <- train(y          = y,
            x          = x, 
            preProcess = NULL,
            method     = catboost.caret, 
            metric     = "MAE", 
            maximize   = FALSE, 
            tuneGrid   = grid, 
            trControl  = control)


#Model information
print(cb)
#0.0419751

#Prediction
cat.pred <- 0.97*predict(cb, properties) + 0.03*0.11
cat.pred = round(cat.pred, 5)

submission = data.frame("parcelid" = as.integer(as.character(properties$parcelid)), 
                        "a_201610" = cat.pred,
                        "a_201611" = cat.pred,
                        "a_201612" = cat.pred,
                        "a_201710" = cat.pred,
                        "a_201711" = cat.pred,
                        "a_201712" = cat.pred)
submission = setDT(submission)
setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))
fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170909_Q_Quick_tuned_catboost.csv")
