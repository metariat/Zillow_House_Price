library(catboost)
library(Metrics) #for mae
library(caret) #for hyperparameter tuning
library(data.table)
library(dplyr)
library(tidyr)

#############################################################

#                    DATA LOADING

#############################################################

path = 'C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/'

train.2016 = fread(paste0(path, 'train_2016_v2.csv'))
train.2017 = fread(paste0(path, 'train_2017.csv'))
train = rbind(train.2016, train.2017)
properties = fread(paste0(path, 'properties_2017.csv'))

train = merge(train, properties, by = "parcelid", all.x = T)


############################################################

#             ADD FEATURES  

############################################################
# water = fread(paste0(path, 'water_distance.csv'))
# density = fread(paste0(path, 'parcel_density.csv'))
# density = subset(density, select = c(parcelid, rank_density3000))
# 
# add.features = function(df){
#   #water distance
#   df = merge(df, water, by.x = 'parcelid', by.y = 'id.parcel', all.x = T)
#   
#   #parcel density
#   df = merge(df, density, by= "parcelid", all.x = T)
#   
# 
# }
# train = add.features(train)
# properties = add.features(properties)



#month
train$transactiondate = as.Date(train$transactiondate, "%Y-%m-%d")
train[, month := month(transactiondate)]

#date
train[, date := format(transactiondate, "%d")]

#quater
train[, quarter := quarter(transactiondate)]




############################################################

#             Missing value

############################################################

#Delete > 0.98%
missing.values <- transpose(data.frame(sapply(properties, function(x) sum(is.na(x))/length(x))))
colnames(missing.values) = colnames(properties)

missing.values <- gather(missing.values, key="feature", value="missing_pct")
missing.values = missing.values[missing.values$missing_pct > 0.98,]
missing.features = as.vector(missing.values$feature)
train[, (missing.features) := NULL]
properties[, (missing.features) := NULL]

#Exclude redundant data
# uniquelength <- sapply(train, function(x) length(unique(x)) == 1)
# Filter(function(x)(length(unique(x))>1), train)





##############################################################
#### Catboost time 
##############################################################
cat.features = c('month', 'date', 'quarter', 
                 'airconditioningtypeid', 'buildingqualitytypeid', 'fips', 
                 'heatingorsystemtypeid', 'propertycountylandusecode', 
                 'propertylandusetypeid', 'regionidcity', 'regionidcounty', 
                 'regionidneighborhood', 'regionidzip', 'yearbuilt')

train[, parcelid := NULL]
train[, transactiondate := NULL]

del.col = c("poolcnt", "fireplaceflag", "assessmentyear", "taxdelinquencyflag", 
            "hashottuborspa", "pooltypeid7", "propertyzoningdesc")
train[, (del.col) := NULL]
properties[, (del.col) := NULL]

train[, censustractandblock := as.numeric(as.character(censustractandblock))]
train <- data.frame(unclass(train))

properties[, censustractandblock := as.numeric(as.character(censustractandblock))]
properties <- data.frame(unclass(properties))

train[is.na(train)] = -999
properties[is.na(properties)] = -999



#Training time
train = setDT(train)
train[, (cat.features) := as.factor(as.character(get(cat.features)))]
train_pool <- catboost.load_pool(data = train[, -c('logerror')], 
                                 label = train$logerror)


properties = setDT(properties)
properties[, month := '12']
properties[, date := '1']
properties[, quarter := '4']
properties[, month := as.factor(month)]
properties[, date := as.factor(date)]
properties[, quarter := as.factor(quarter)]

test_pool <- catboost.load_pool(data=properties, label = NULL)

cat.pred = rep(0, nrow(properties))
for( i in 1:100){
  set.seed(i)
  fit.params = list(iterations=200, learning_rate=0.03,
                 depth=6, l2_leaf_reg=3,
                 loss_function='MAE',
                 eval_metric='MAE')
  model = catboost.train(train_pool, params = fit.params)
  cat.pred = cat.pred + catboost.predict(model, test_pool)
}
cat.pred = cat.pred / 100
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
fwrite(submission, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20171005_catboost_raw_data.csv")
