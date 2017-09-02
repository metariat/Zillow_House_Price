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
path = 'C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data'
transactions = fread(paste0(path, "/train_2016_v2.csv"))


#Read the cleaned properties data
properties = readRDS(paste0(path, "/properties_v2.RDS"))

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



#Replace Inf values
invisible(lapply(names(properties),function(.name) set(properties, which(is.infinite(properties[[.name]])), j = .name,value =9999)))
properties <- setDT(as.data.frame(unclass(properties)))

str(properties)
properties[, use.1 := ifelse(use.1 == TRUE, 1, 0)]
properties[, use.2 := ifelse(use.2 == TRUE, 1, 0)]
properties[, use.3 := ifelse(use.3 == TRUE, 1, 0)]
properties[, use.4 := ifelse(use.4 == TRUE, 1, 0)]

properties[, tract.number := as.numeric(as.character(tract.number))]

properties = model.matrix(~., properties)
saveRDS(properties, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_v2_sparse_matrix.RDS")


### Merge with the transaction data
data = merge(transactions, properties, by = "parcelid", all.x = T)
dim(data)
set.seed(1)
train.index = sample(1:nrow(data), size = round(0.7 * nrow(data)), replace = FALSE)
train = data[train.index ,]
test = data[-train.index ,]

saveRDS(train, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train.RDS")
saveRDS(test, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/test.RDS")
