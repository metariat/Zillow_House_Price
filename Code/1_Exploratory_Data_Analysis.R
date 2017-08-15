#-----------------------------------------------------------------------------#
#---                     Package loadings                                  ---#
#-----------------------------------------------------------------------------#
options( warn = -1 )
library(data.table)
library(dplyr)
library(lubridate) #working with date
library(ggplot2)
library(tidyr)
library(ggmap) #plot google map
library(geonames) #API for calculating the altitude
library(geosphere) #calculate geospatial distance between two points

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



#'''where are the houses?

test = properties[sample(500), ]
df = data.frame(lon = test$longitude, lat = test$latitude)

qmplot(lon, lat, data = df, colour = I('red'), maptype = "watercolor", zoom = 12)
#''' Ideas for feature engineering: 
#'''     - Calculate the distance to the beach
#'''     - Calculate the altitude of each point
#'''     - calculate the distance to the forest
#'''     - Those two features should be very important
#''' Remark & take away: some houses are situated on an island near by, 
#'''     thus need a special treatment



#-----------------------------------------------------------------------------#
#---                     Feature Engineering                               ---#
#-----------------------------------------------------------------------------#

#---------------------
#-- Altitude
#---------------------

#'''Using google API is paying: https://developers.google.com/maps/documentation/elevation/start?hl=fr
#'''Alternative: using the geonames package
library(geonames)
options(geonamesUsername="quang")
#Using the srtm3 digital elevation model:
#Attention: hourly limit of 2000 credits
GetAl = function(lat, long){
  return(GNsrtm3(lat, long)$srtm3)
}
properties$altitude1 = mapply(GetAl, long = properties$longitude, lat = properties$latitude)



#---------------------
#-- Calculate the distance to the beach
#---------------------
#'''Idea: the beach borders' longitude,altitude can be approximately estimated by the informations
#''' of the most left house for each range of longitude
properties$long.percentile = cut(properties$longitude, 
                                  unique(quantile(properties$longitude, probs=0:1000/1000, na.rm = T)), 
                                  include.lowest=TRUE, 
                                  labels=FALSE)
#Calculate the min in each percentile group
properties[, lat.min := min(latitude), by = long.percentile]
#check
test = data.frame(unique(properties[, c("longitude", "lat.min")]))
test = test[sample(1000), ]
qmplot(longitude, lat.min, data = test, colour = I('red'), maptype = "watercolor", zoom = 9)
#still some unexpected point, we can reduce the number of percentile to eliminate those point.
#but the trade-off will be that the distance would be less accurate for other points

#Calculate the distance
for (i in 1:nrow(properties)){
  properties$water.distance[i] = distHaversine(c(properties$longitude[i], properties$latitude[i]), 
                                            c(properties$longitude[i], properties$lat.min[i]))
}

