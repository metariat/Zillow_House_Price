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


#Read the data
path = 'C:/Quang/Kaggle/Zillow_House_Price_Data/'
transactions = fread(paste0(path, "/train_2016_v2.csv"))
properties = fread(paste0(path, "properties_2016.csv"))
sample.submission = fread(paste0(path, "sample_submission.csv"))

#Formatting the data
transactions$transactiondate = as.Date(transactions$transactiondate, "%Y-%m-%d")
properties$longitude = properties$longitude / 1e06
properties$latitude = properties$latitude / 1e06


#Transforming look-alike continuous variable to categorical variables:
cat.var = c("airconditioningtypeid", "architecturalstyletypeid", "buildingclasstypeid",
            "decktypeid", "heatingorsystemtypeid", "propertylandusetypeid", "regionidcounty",
            "regionidcity", "regionidzip", "regionidneighborhood", "storytypeid", 
            "typeconstructiontypeid", "fips")

for (i in cat.var){
  properties[, eval(i):= as.factor(as.character(get(i)))]
}


#-----------------------------------------------------------------------------#
#---                     Exploratory Data Analysis                         ---#
#-----------------------------------------------------------------------------#



#---------------------    1.Targer variable
#---------------------------------------------------

#'''First look at the response variable:
hist(transactions$logerror, breaks = 1000, xlab = "logerror", ylab = "frequency",
    main = "histogram of the logerror")

#'''The min and the max are far away -> possible outlier
#'''Takeaway 1: Capping / Collaring could improve the model later on.


#Log error over time
transactions <- transactions %>% mutate(abs.logerror = abs(logerror))
transactions %>% 
  mutate(year.month = make_date(year = year(transactiondate), month = month(transactiondate))) %>% 
  group_by(year.month) %>% summarize(mean.abs.logerror = mean(abs.logerror)) %>% 
  ggplot(aes(x = year.month, y = mean.abs.logerror)) + 
  geom_line(size = 1.5, color="red") +
  geom_point(size = 5, color="red") + theme_bw()

#'''Quick conclusion:
#'''  The predictions of Zillow gets better over time
#'''  But why there is a peak after October 2016?
#'''  May be it is just a seasonality effect, not the prediction quality
#'''Thoughful conclusion:
#'''  The time interval is just one year, it could be just a seasonality effect, 
#'       not an improvement of Zillow's model
#'''  We don't know that, hence to be check when the 2017 data is available
#'''Takeaway 2: Change the weight of the observations at the same period of the 
#'''testing set would be beneficial, not many people will remark that, so it could be 
#'''the key point to progress on the leaderboad. So let's hope that it's a seasonality 
#'''effect If it is a seasonality effect, we will give more weight to the observations 
#'''at the same perid as the testing set





#---------------------    2.Predictors
#---------------------------------------------------



#--------------------- 2.1 Missing value

#'''Missing value
missing.values <- transpose(data.frame(sapply(properties, function(x) sum(is.na(x))/length(x))))
colnames(missing.values) = colnames(properties)

missing.values <- gather(missing.values, key="feature", value="missing_pct")
missing.values %>% 
  ggplot(aes(x=reorder(feature,missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red") +
  coord_flip()+theme_bw()

#''' A lot of variable has high level of missing value, around 100%. We could drop those 
#''' variables at the first tentative, then if the results are not satisfying, we come back






#--------------------- 2.2 Categorical variables
#Merge transactions and properties data
data = merge(transactions, properties, by = "parcelid", all.x = T)

cat.var = c("airconditioningtypeid",
            "decktypeid",
            "regionidcity",
            "typeconstructiontypeid",
            "architecturalstyletypeid",
            "heatingorsystemtypeid",
            "regionidzip",
            "buildingclasstypeid",
            "propertylandusetypeid",
            "regionidneighborhood",
            "regionidcounty",
            "storytypeid")

data= setDT(data)
for (i in cat.var){
  print(i)
  data[, eval(i):= as.factor(as.character(get(i)))]
}

for (i in names(Filter(is.factor, data))){
p1 = ggplot(data, aes(x = get(i), y = logerror, fill = get(i))) +
      geom_violin() +
      geom_boxplot(width=0.1, fill="white") +
      scale_colour_continuous(guide = FALSE) +
      scale_y_continuous(limits = c(-0.2, 0.2)) +
      ggtitle(paste0("Plot of ", i))
p2 = ggplot(data, aes(x = get(i))) + geom_bar()
jpeg(file = paste0("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Graphics/EDA_", i, ".png")
     , width = 1000, height = 682)
grid.arrange(p1, p2, nrow=2)
dev.off()
}

#Wow, their model is very good



#'''where are the houses?

test = properties[sample(2000), ]
df = data.frame(lon = test$longitude, lat = test$latitude)

qmplot(lon, lat, data = df, colour = I('red'), maptype = "watercolor", zoom = 12)
#''' Ideas for feature engineering: 
#'''     - Calculate the distance to the beach
#'''     - Calculate the altitude of each point
#'''     - calculate the distance to the forest
#'''     - Those two features should be very important
#''' Remark & take away: some houses are situated on an island near by, 
#'''     thus need a special treatment







