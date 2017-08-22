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
transactions = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train_2016_v2.csv")
properties = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_2016.csv")
sample.submission = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/sample_submission.csv")

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

#Transforming bad formatting categorical variables
properties$fireplaceflag = ifelse(properties$fireplaceflag == "true", 1, 0)
properties[, pooltypeid2 := ifelse(pooltypeid2 == 1, 1, 
                                   ifelse(poolcnt == 1,0, -1))]
properties[, pooltypeid7 := ifelse(pooltypeid7 == 1, 1, 
                                   ifelse(poolcnt ==1, 0, -1))]
properties[, pooltypeid10 := ifelse(pooltypeid10 == 1, 1, 0)]
properties[, hashottuborspa := ifelse(hashottuborspa == "true", 1, 0)]
properties <- properties %>% 
                mutate(census = as.character(rawcensustractandblock), 
                       tract.number = str_sub(census,5,11), 
                       tract.block = str_sub(census,12))


#Some varialbes in which missing value is not really missing

properties = setDT(properties) 
properties[, airconditioningtypeid := ifelse(is.na(airconditioningtypeid), "-1", 
                                             as.character(airconditioningtypeid))]

properties[, is.fire.place := ifelse(is.na(fireplacecnt), 0, 1)]

properties[, fireplacecnt := ifelse(is.na(fireplacecnt), 0, fireplacecnt)]

properties[, poolcnt := ifelse(is.na(poolcnt), 0, poolcnt)]

properties[, is.city := ifelse(is.na(regionidcity), 0, 1)]

properties[, decktypeid := ifelse(is.na(decktypeid), "-1", as.character(decktypeid))]

properties[, storytypeid := ifelse(is.na(storytypeid), "-1", as.character(storytypeid))] #type of house (if contain "sous-sol", "grenier", etc.)

yardbuildingsqft26.NA.type = ifelse(is.na(properties$yardbuildingsqft26 ), -1, 0) # -1: NA, 0: having value 
properties[, yardbuildingsqft26 := ifelse(is.na(yardbuildingsqft26), mean(yardbuildingsqft26, na.rm = TRUE), yardbuildingsqft26)] #surface of the storage in the yard

yardbuildingsqft17.NA.type = ifelse(is.na(properties$yardbuildingsqft17 ), -1, 0) # -1: NA, 0: having value
properties[, yardbuildingsqft17 := ifelse(is.na(yardbuildingsqft17), mean(yardbuildingsqft17, na.rm = TRUE), yardbuildingsqft17)] #surface of the patio in the yard

#renaming the columns
properties <- properties %>% rename(
  id.parcel = parcelid,
  build.year = yearbuilt,
  area.basement = basementsqft,
  area.patio = yardbuildingsqft17,
  area.shed = yardbuildingsqft26, 
  area.pool = poolsizesum,  
  area.lot = lotsizesquarefeet, 
  area.garage = garagetotalsqft,
  area.firstfloor.finished = finishedfloor1squarefeet,
  area.total.calc = calculatedfinishedsquarefeet,
  area.base = finishedsquarefeet6,
  area.live.finished = finishedsquarefeet12,
  area.liveperi.finished = finishedsquarefeet13,
  area.total.finished = finishedsquarefeet15,  
  area.unknown = finishedsquarefeet50,
  num.unit = unitcnt, 
  num.story = numberofstories,  
  num.room = roomcnt,
  num.bathroom = bathroomcnt,
  num.bedroom = bedroomcnt,
  num.bathroom.calc = calculatedbathnbr,
  num.bath = fullbathcnt,  
  num.75.bath = threequarterbathnbr, 
  num.fireplace = fireplacecnt,
  num.pool = poolcnt,  
  num.garage = garagecarcnt,  
  region.county = regionidcounty,
  region.city = regionidcity,
  region.zip = regionidzip,
  region.neighbor = regionidneighborhood,  
  tax.total = taxvaluedollarcnt,
  tax.building = structuretaxvaluedollarcnt,
  tax.land = landtaxvaluedollarcnt,
  tax.property = taxamount,
  tax.year = assessmentyear,
  tax.delinquency = taxdelinquencyflag,
  tax.delinquency.year = taxdelinquencyyear,
  zoning.property = propertyzoningdesc,
  zoning.landuse = propertylandusetypeid,
  zoning.landuse.county = propertycountylandusecode,
  flag.fireplace = fireplaceflag, 
  flag.tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural.style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id.parcel = parcelid,
  date = transactiondate
)







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
  mutate(year.month = make_date(year = year(date), month = month(date))) %>% 
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
data = merge(transactions, properties, by = "id.parcel", all.x = T)
sum(is.na(data$flag.fireplace)) #check ok


cat.var = c("aircon",
            "deck",
            "region.city",
            "material",
            "architectural.style",
            "heating",
            "region.zip",
            "framing",
            "zoning.landuse",
            "region.neighbor",
            "region.county",
            "story")


for (i in cat.var){
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







