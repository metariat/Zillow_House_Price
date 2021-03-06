library(catboost)
library(Metrics) #for mae
library(caret) #for hyperparameter tuning
library(data.table)
gc()

#---------------------------------------------------------------#

#            Data Loading

#---------------------------------------------------------------#
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

train$transactiondate = as.Date(train$transactiondate, "%Y-%m-%d")
test$transactiondate = as.Date(test$transactiondate, "%Y-%m-%d")

train[, month := month(transactiondate)]
test[, month :=month(transactiondate)]

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
                        number = 4,
                        verboseIter=TRUE,
                        summaryFunction = maeSummary)

grid <- expand.grid(depth = c(5),
                    learning_rate = c(0.06, 0.05),
                    iterations = c(250),
                    l2_leaf_reg = c(1e-3, 1.5e-3, 0.5e-3),
                    rsm = c(0.5),
                    border_count = c(64))

time1 = Sys.time()
cb <- train(y          = y.train,
            x          = x.train, 
            preProcess = NULL,
            method     = catboost.caret, 
            metric     = "MAE", 
            maximize   = FALSE, 
            tuneGrid   = grid, 
            trControl  = control)
time2 = Sys.time()

#learning rate = 0.04, iterations = 1000, l2_leaf_reg = 1e-3, rms = 0.4, border_count = 64
# Fitting depth = 5, learning_rate = 0.04, iterations = 1500, l2_leaf_reg = 0.001, rsm = 0.4, border_count = 64 on full training set

#Fitting depth = 5, learning_rate = 0.06, iterations = 250, l2_leaf_reg = 0.001, rsm = 0.4, border_count = 64 on full training set

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
