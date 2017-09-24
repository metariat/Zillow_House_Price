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


#-----------------------------------------------------------#

train_pool <- catboost.load_pool(data = x.train, label = y.train)
test_pool <- catboost.load_pool(data = x.test, label = y.test)

params_simple <- list(depth = 5,
                      learning_rate = 0.04,
                      iterations = 400,
                      l2_leaf_reg = 1e-3,
                      rsm = 0.4,
                      border_count = 64,
                      loss_function = 'MAE',
                      train_dir = 'train_dir')
model_simple <- catboost.train(train_pool, test_pool, params_simple)

params_with_od <- list(depth = 5,
                       learning_rate = 0.04,
                       iterations = 400,
                       l2_leaf_reg = 1e-3,
                       rsm = 0.4,
                       border_count = 64,
                       loss_function = 'MAE',
                       train_dir = 'train_dir',
                       od_type = 'Iter',
                       od_wait = 50)
model_with_od <- catboost.train(train_pool, test_pool, params_with_od)
model_with_od$tree_count

params_best <- list(depth = 5,
                    learning_rate = 0.04,
                    iterations = 400,
                    l2_leaf_reg = 1e-3,
                    rsm = 0.4,
                    border_count = 64,
                    loss_function = 'MAE',
                    train_dir = 'train_dir',
                    use_best_model = TRUE)
model_best <- catboost.train(train_pool, test_pool, params_best)

prediction_simple <- catboost.predict(model_simple, test_pool)
prediction_best <- catboost.predict(model_best, test_pool)
prediction_od <- catboost.predict(model_with_od, test_pool)

mae(y.test, prediction_simple)
mae(y.test, prediction_best)
mae(y.test, prediction_od)








