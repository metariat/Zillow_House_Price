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

properties[, fips := ifelse(is.na(fips), "6037", as.character(fips))]
del.col = c("tract.block", "regionidzip", "regionidcity", "censustractandblock", 
            "census", "rawcensustractandblock", "propertyzoningdesc", 
            "propertycountylandusecode", "regionidneighborhood")
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
rm(properties)
gc()

#Check the redundant variables
l <- lapply(data, function(X) as.numeric(factor(X, levels=unique(X))))
m <- as.matrix(data.frame(l))
M <- (cor(m,m)==1)
M[is.na(M)] = FALSE
M[lower.tri(M, diag=TRUE)] <- FALSE
colnames(M)[colSums(M)>0]
#OK

train.index = sample(1:nrow(data), size = round(0.7 * nrow(data)), replace = FALSE)
train = data[train.index ,]
test = data[-train.index ,]

saveRDS(train, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train.RDS")
saveRDS(test, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/test.RDS")
