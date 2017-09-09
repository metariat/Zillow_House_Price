library(catboost)
library(Metrics) #for mae
library(caret) #for hyperparameter tuning


#---------------------------------------------------------------#

#            Data Loading

#---------------------------------------------------------------#
source("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Code/0_Data_Loading_For_Modelling.R")
parcelid = properties$parcelid

#-------------------------------------------------------------------#

#                   Defining data set

#-------------------------------------------------------------------#
train = train[abs(train$logerror) < 0.2]
test = test[abs(test$logerror) < 0.2]

y = c(train$logerror, test$logerror)
x = rbind(train, test)
x = subset(x, select = -c(logerror, parcelid))




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

grid <- expand.grid(depth = c(6),
                    learning_rate = c(0.0021),
                    iterations = c(600),
                    l2_leaf_reg = c(1e-3),
                    rsm = c(0.95),
                    border_count = c(32)
)

cb <- train(y=y,
            x=x, 
            preProcess=NULL,
            method=catboost.caret, 
            metric = "MAE", 
            maximize = FALSE, 
            tuneGrid = grid, 
            trControl = control
)


#variable importance
importance <- varImp(cb, scale = FALSE)
print(importance)
plot(importance)

#training mae
cat.pred <- predict(cb, x)
mae(cat.pred, y)

#making cat.pred
cat.pred = predict(cb, properties)
cat.pred = round(cat.pred, 5)
result <- data.frame(cbind(parcelid, cat.pred, cat.pred, cat.pred, cat.pred, cat.pred, cat.pred))
colnames(result) <- c("parcelid","201610","201611","201612","201710","201711","201712")
result$parcelid = as.integer(as.character(result$parcelid))
fwrite(result, 
       file = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170902_Q_init_catboost_w_o_outliers_trans_var.csv", 
       row.names = FALSE ) 
