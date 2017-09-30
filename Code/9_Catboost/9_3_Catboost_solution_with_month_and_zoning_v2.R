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

zoning = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/Cat_residuals.csv16_710obs_cluster (2).csv")
zoning = subset(zoning, select = c(zip, beta_combination))
zoning[, beta_combination := ifelse(beta_combination == 1, 2, 
                                    ifelse(beta_combination == 5, 4, beta_combination))]
#renaming
setnames(train, old = "N.prop.typeNot Built", new = "N.prop.typeNot.Built")
setnames(test, old = "N.prop.typeNot Built", new = "N.prop.typeNot.Built")
setnames(properties, old = "X.Intercept.", new = "(Intercept)")

#month calculation
train$transactiondate = as.Date(train$transactiondate, "%Y-%m-%d")
test$transactiondate = as.Date(test$transactiondate, "%Y-%m-%d")

train[, month := month(transactiondate)]
test[, month :=month(transactiondate)]

train[, transactiondate := NULL]
test[, transactiondate := NULL]


train = merge(train, zoning, by.x = "tract.number", by.y = "zip", all.x = T)
test = merge(test, zoning, by.x = "tract.number", by.y = "zip", all.x = T)
properties = merge(properties, zoning, by.x = "tract.number", by.y = "zip", all.x = T)
gc()

train[, beta_combination := ifelse(is.na(beta_combination),
                                   median(beta_combination, na.rm = T),
                                   beta_combination)]

test[, beta_combination := ifelse(is.na(beta_combination),
                                   median(beta_combination, na.rm = T),
                                   beta_combination)]

properties[, beta_combination := ifelse(is.na(beta_combination),
                                   median(beta_combination, na.rm = T),
                                   beta_combination)]

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
rm(properties)
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
                        number = 5,
                        verboseIter=TRUE,
                        summaryFunction = maeSummary)

grid <- expand.grid(depth = c(5),
                    learning_rate = c(0.06),
                    iterations = c(400),
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
            trControl  = control)


#Model information
print(cb)
#0.04183

#Prediction
gc()
test.pred = predict(cb, x.test)
mean(abs(test.pred - y.test))
#0.06633935
