library(data.table)
gc()

#-------------------------------------------------------------------#

# Solution without outliers capping = 0.2 & transformed variables

#-------------------------------------------------------------------#


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
train = train[abs(train$logerror) < 0.4, ]

train[, transactiondate := NULL]
test[, transactiondate := NULL]
train.tract.number = train$tract.number
test.tract.number = test$tract.number
train[, tract.number := NULL]
test[, tract.number := NULL]
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



#----- Applying a simple XGBoost for extracting the residuals(not optimized parameters)
#---------------------------------------------------------------------------------------
y.train = train$logerror
x.train = subset(train, select= -c(logerror, parcelid))
x.train = as.matrix(x.train)

y_mean = mean(y.train)

param <- list(objective           = "reg:linear",
              eval_metric         = "mae",
              eta                 = 0.037,
              max_depth           = 5, 
              subsample           = 0.8,
              colsample_bytree    = 0.5,
              min_child_weight    = 4,
              maximize            = FALSE,
              lambda              = 0.8,
              alpha               = 0.4,
              base_score          = y_mean,
              silent              = 0)

xgb_cv <- xgb.cv(data = x.train,
                 label = y.train,
                 params = param,
                 nrounds = 1000,
                 prediction = TRUE,
                 maximize = FALSE,
                 nfold = 5,
                 print_every_n = 5)


plot(xgb_cv$evaluation_log$train_mae_mean, type = "l")
lines(xgb_cv$evaluation_log$test_mae_mean, type = "l", col = "red")

print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]) #0.0402418
round = xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]$iter

dtrain = xgb.DMatrix(as.matrix(x.train),label = y.train)

model = xgb.train(param, data = dtrain, nrounds = round)
importance <- xgb.importance(colnames(dtrain), model = model)
xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative importance")


#Extract
xgb.pred = predict(model, new = x.train)

data = data.frame("pred"= xgb.pred, "obs" = y.train, 
                  "x" = train$longitude, "y" = train$latitude,
                  "insee" = train.tract.number)
data[, random := sample.int(10, nrow(data), replace = T)]
data[, exposure := 1]
fwrite(data, "Z:/Users/Q.DO/Poubelle/XGB_residuals.csv")
