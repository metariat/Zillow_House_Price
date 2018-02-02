library(data.table)



#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data reading                                      #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

#test github

train = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/train.RDS"))
test = setDT(readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/test.RDS"))

train[, transactiondate := NULL]
test[, transactiondate := NULL]

heavi.tail = c("basementsqft",
               "bathroomcnt",
               "bedroomcnt",
               "calculatedbathnbr",
               "finishedfloor1squarefeet",
               "calculatedfinishedsquarefeet",
               "fireplacecnt",
               "fullbathcnt",
               "garagecarcnt",
               "garagetotalsqft",
               "lotsizesquarefeet",
               "poolcnt",
               "poolsizesum",
               "roomcnt",
               "threequarterbathnbr",
               "unitcnt",
               "numberofstories",
               "fireplaceflag",
               "structuretaxvaluedollarcnt",
               "taxvaluedollarcnt",
               "landtaxvaluedollarcnt",
               "taxamount",
               "taxdelinquencyflagY",
               "taxdelinquencyyear",
               "is.fire.place",
               "is.city",
               "N.living.area.error",
               "N.living.area.prop",
               "N.ExtraSpace",
               "N.TotalRooms",
               "N.AvRoomSize",
               "N.ExtraRooms",
               "N.ValueProp",
               "N.GarPoolAC",
               "N.value.ratio",
               "N.tax.score",
               "N.life.tax",
               "N.city.count",
               "N.tract.count",
               "N.prop.typeMixed",
               "N.prop.typeOther",
               "water.distance")


for (i in heavi.tail){
  min = min(c(train[, get(i)], test[, get(i)]), 0)
  train[, eval(i):= log(get(i) - min + 1)]
  test[, eval(i):= log(get(i) - min + 1)]
}


#-------------------------------------------------------------------#
# Model without outliers #
#-------------------------------------------------------------------#


n = 0
cv.score = rep(0,100)
for (i in seq(0.1, 1, length=100)){
  n = n + 1
  train1 = train[abs(train$logerror) < i, ]
  y.train = train1$logerror
  x.train = subset(train1, select= -c(logerror))
  x.train = as.matrix(x.train)
  
  y.test = test$logerror
  x.test = subset(test, select= -c(logerror))
  x.test = as.matrix(x.test)
  
  
  
  #Model
  options(scipen = 999)
  library(glmnet)
  cv.out = cv.glmnet(x.train, y.train, alpha = 0, nfolds = 10, type.measure = "mae", parallel = T) 
  plot(cv.out)
  bestlam = cv.out$lambda.min
  ridge.pred = predict(cv.out, s = bestlam, newx=x.test)
  cv.score[n] = mean(abs(ridge.pred - y.test))
}

plot(x = seq(0.1, 1, length=100), y = cv.score, type = "l", 
     xlab = "outlier threshold",
     ylab = "value of test MAE when train the model below the threshold")
