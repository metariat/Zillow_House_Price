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

y.test = test$logerror
x.test = subset(test, select= -c(logerror, parcelid))




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
                        summaryFunction = maeSummary
)

grid <- expand.grid(depth = c(5),
                    learning_rate = c(0.0037, 0.005),
                    iterations = c(600),
                    l2_leaf_reg = c(1e-3),
                    rsm = c(0.5, 0.9),
                    border_count = c(32)
)
#0.04226789
cb <- train(y          = y.train,
            x          = x.train, 
            preProcess = NULL,
            method     = catboost.caret, 
            metric     = "MAE", 
            maximize   = FALSE, 
            tuneGrid   = grid, 
            trControl  = control
)


#Model information
print(cb)

#variable importance
importance <- varImp(cb, scale = FALSE)
print(importance)
plot(importance)

#training mae
cat.pred <- predict(cb, x.train)
mae(cat.pred, y.train)

#testing mae
cat.pred <- predict(cb, x.test)
mae(cat.pred, y.test)
