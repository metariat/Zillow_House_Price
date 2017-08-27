#-----------------------------------------------------------------------------#
#---                     Package loadings                                  ---#
#-----------------------------------------------------------------------------#
options(warn = -1 )
library(data.table)
library(dplyr)
library(lubridate) #working with date
library(ggplot2)
library(tidyr)
library(ggmap) #plot google map
library(geonames) #API for calculating the altitude
library(geosphere) #calculate geospatial distance between two points
library(randomForest) #For the first benchmark model
library(glmnet) #penalized regression models
library(xgboost) #Gradient Boosting
library(Rcpp) #C++ interface
library(gridExtra) #side by side ggplot
library(stringr) #regrex

#-----------------------------------------------------------------------------#
#---                     Data loadings                                     ---#
#-----------------------------------------------------------------------------#


#Read the data from Kaggle
path = 'C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/'
transactions = fread(paste0(path, "/train_2016_v2.csv"))


#Read the cleaned properties data
properties = readRDS("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_v2.RDS")

fwrite(properties, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_v2.csv")


properties[, fips := ifelse(is.na(fips), "6037", as.character(fips))]

del.col = c("tract.block", "regionidzip", "regionidcity", "censustractandblock", "census", "rawcensustractandblock")
properties[ ,(del.col) := NULL]


#Missing value check
missing.values <- transpose(data.frame(sapply(properties, function(x) sum(is.na(x))/length(x))))
colnames(missing.values) = colnames(properties)

missing.values <- gather(missing.values, key="feature", value="missing_pct")
missing.values %>% 
  ggplot(aes(x=reorder(feature,missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red") +
  coord_flip()+theme_bw()
#OK

data = merge(transactions, properties, by = "parcelid", all.x = T)
del.col = c("tract.block", "regionidzip", "regionidcity", "censustractandblock", 
            "census", "rawcensustractandblock", "transactiondate", "parcelid", 
            "propertyzoningdesc", "regionidneighborhood", "propertycountylandusecode", "calculatedbathnbr.NA.type",
            "taxdelinquencyyear.NA.type",
            "N.life.tax",
            "finishedfloor1squarefeet.NA.type",
            "basementsqft.NA.type",
            "N.tract.count")
data[ ,(del.col) := NULL]

data <- as.data.frame(unclass(data))
data[mapply(is.infinite, data)] <- 9999



#Check the redundant variables
l <- lapply(data, function(X) as.numeric(factor(X, levels=unique(X))))
m <- as.matrix(data.frame(l))
M <- (cor(m,m)==1)
M[is.na(M)] = FALSE
M[lower.tri(M, diag=TRUE)] <- FALSE
colnames(M)[colSums(M)>0]


data.w.o.outlier = data[abs(data$logerror) < 1.5, ]
rm(properties)

x = subset(data.w.o.outlier, select = -c(logerror) )
x = model.matrix(logerror ~ ., data.w.o.outlier)
y = data.w.o.outlier$logerror

crossval <-  cv.glmnet(x = x, y = y)
plot(crossval)
penalty <- crossval$lambda.min #optimal lambda
fit1 <-glmnet(x = x, y = y, alpha = 1, lambda = penalty ) #estimate the model with that
coef(fit1)

y.hat = predict(fit1, newx = x)
mean(abs(y - y.hat))

del.col = c("tract.block", "regionidzip", "regionidcity", "censustractandblock", 
            "census", "rawcensustractandblock", "transactiondate", "parcelid", 
            "propertyzoningdesc", "regionidneighborhood", "propertycountylandusecode", "calculatedbathnbr.NA.type",
            "taxdelinquencyyear.NA.type",
            "N.life.tax",
            "finishedfloor1squarefeet.NA.type",
            "basementsqft.NA.type",
            "N.tract.count")
properties[ ,(del.col) := NULL]

properties <- as.data.frame(unclass(properties))
properties[mapply(is.infinite, properties)] <- 9999



dtest = model.matrix( ~., properties[1:1000])
y.hat = predict(fit1, newx = test)

