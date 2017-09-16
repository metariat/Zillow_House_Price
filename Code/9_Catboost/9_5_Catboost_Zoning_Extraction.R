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
tract = train$tract.number
train[, tract.number := NULL]
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid, 
                                   longitude, latitude, fips6059, fips6111,
                                   N.zip.count, N.city.count, N.region.count,
                                   regionidcounty2061, regionidcounty3101,
                                   N.tract.count, water.distance, is.city,
                                   transactiondate))

train$transactiondate = as.Date(train$transactiondate, "%Y-%m-%d")
train[, month := month(transactiondate)]
train[, transactiondate := NULL]
rm(properties)
rm(test)
gc()

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
                        number = 3,
                        verboseIter=TRUE,
                        summaryFunction = maeSummary)

grid <- expand.grid(depth = c(5),
                    learning_rate = c(0.06),
                    iterations = c(600),
                    l2_leaf_reg = c(1e-3),
                    rsm = c(0.6),
                    border_count = c(64))
cb <- train(y          = y.train,
            x          = x.train, 
            preProcess = NULL,
            method     = catboost.caret, 
            metric     = "MAE", 
            maximize   = FALSE, 
            tuneGrid   = grid, 
            trControl  = control
)

#second round
# depth learning_rate iterations l2_leaf_reg rsm border_count
# 7     5          0.01        600       0.001 0.5           32


#third round
# depth learning_rate iterations l2_leaf_reg rsm border_count
# 10     5          0.05        600       0.001 0.5           64

#forth round
# depth learning_rate iterations l2_leaf_reg rsm border_count
# 4     5          0.07        600       0.001 0.7           64

# 5 round
# depth learning_rate iterations l2_leaf_reg rsm border_count
# 3     5          0.06        600       0.001 0.6           64



#Model information
print(cb)
#0.04219282

#variable importance
importance <- varImp(cb, scale = FALSE)
print(importance)
plot(importance)

#training mae
cat.pred <- predict(cb, x.train)



data = data.frame("insee" = tract, 'X' = train$longitude, 'Y' = train$latitude, 'Weight' = 1, 'obs' = y.train, 'pred' = cat.pred)
data$random = sample.int(10, nrow(data), replace = T)
fwrite(data, paste0(path, "Cat_residuals.csv"), sep = ";")
