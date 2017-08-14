#-----------------------------------------------------------------------------#
#---                     Package loadings                                  ---#
#-----------------------------------------------------------------------------#
options( warn = -1 )
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


#-----------------------------------------------------------------------------#
#---                     Data loadings                                     ---#
#-----------------------------------------------------------------------------#


#Read the data
transactions = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Data/train_2016_v2.csv")
properties = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Data/properties_2016.csv")
sample.submission = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Data/sample_submission.csv")

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

#Formatting the data
transactions$date = as.Date(transactions$date, "%Y-%m-%d")


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
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

gf










#-----------------------------------------------------------------------------#
#---                     Feature Engineering                               ---#
#-----------------------------------------------------------------------------#

