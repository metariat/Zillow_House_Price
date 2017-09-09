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

zoning = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/Zonier_XGB.csv")
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
gc()
properties[, month := 10]
cat.pred.10 <- 0.97*predict(cb, properties) + 0.03*0.11

gc()
properties[, month := 11]
cat.pred.11 <- 0.97*predict(cb, properties) + 0.03*0.11

gc()
properties[, month := 12]
cat.pred.12 <- 0.97*predict(cb, properties) + 0.03*0.11
gc()

cat.pred.10 = round(cat.pred.10, 5)
cat.pred.11 = round(cat.pred.11, 5)
cat.pred.12 = round(cat.pred.12, 5)

submission = data.frame("parcelid" = as.integer(as.character(properties$parcelid)), 
                        "a_201610" = cat.pred.10,
                        "a_201611" = cat.pred.11,
                        "a_201612" = cat.pred.12,
                        "a_201710" = cat.pred.10,
                        "a_201711" = cat.pred.11,
                        "a_201712" = cat.pred.12)
submission = setDT(submission)
setnames(submission, old = c("a_201610", "a_201611", "a_201612", "a_201710", "a_201711", "a_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))
fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170909_Q_Quick_tuned_catboost_with_month_with_zoning.csv")
