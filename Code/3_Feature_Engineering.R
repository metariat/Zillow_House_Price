#---------------------
#-- Creating additional variables
#---------------------
# A lot of additional variables can be found here:
#   https://www.kaggle.com/nikunjm88/creating-additional-features




#----------                         Features relates to the property
#----------------------------------------------------------------------

#life of the house
properties[, N.life := 2018 - yearbuilt]

#error of calculation of finished area
properties[, N.living.area.error := calculatedfinishedsquarefeet / finishedsquarefeet12]
properties[, N.living.area.prop := calculatedfinishedsquarefeet / lotsizesquarefeet]

properties[, N.LivingAreaProp2 := finishedsquarefeet12 / finishedsquarefeet15]

#Amout of extra space
properties[, N.ExtraSpace := lotsizesquarefeet -calculatedfinishedsquarefeet]
properties[, N.ExtraSpace.2 := finishedsquarefeet15 - finishedsquarefeet12]

#Total number of rooms
properties[, N.TotalRooms := bathroomcnt * bedroomcnt]

#Average room size
properties[, N.AvRoomSize := calculatedfinishedsquarefeet / roomcnt]

# Number of Extra rooms
properties[, N.ExtraRooms := roomcnt - N.TotalRooms]

#Ratio of the built structure value to land area
properties[, N.ValueProp := structuretaxvaluedollarcnt / landtaxvaluedollarcnt]

#Does property have a garage, pool or hot tub and AC?
properties[, N.GarPoolAC := ifelse((garagecarcnt > 0) & (pooltypeid10 > 0) & (airconditioningtypeid!=5), 1, 0 )]







#---------- Tax related variables
#----------------------------------------------------------------------


#Ratio of tax of property over parcel
properties[, N.value.ratio := taxvaluedollarcnt / taxamount]

#TotalTaxScore
properties[, N.tax.score := taxvaluedollarcnt * taxamount]

#Length of time since unpaid taxes
properties[, N.life.tax := 2018 - taxdelinquencyyear]







#----------           Other features based off the location
#----------------------------------------------------------------------
#Number of properties in the zip
test = data.frame(table(properties$regionidzip))
test = test %>% rename(
  regionidzip = Var1,
  N.zip.count = Freq
)
properties = merge(properties, test, by = "regionidzip", all.x = T)

#Number of properties in the city
test = data.frame(table(properties$regionidcity))
test = test %>% rename(
  regionidcity = Var1,
  N.city.count = Freq
)
properties = merge(properties, test, by = "regionidcity", all.x = T)

#Number of properties in the region
test = data.frame(table(properties$regionidcounty))
test = test %>% rename(
  regionidcounty = Var1,
  N.region.count = Freq
)
properties = merge(properties, test, by = "regionidcounty", all.x = T)


#Number of properties per tract number
test = data.frame(table(properties$tract.number))
test = test %>% rename(
  tract.number = Var1,
  N.tract.count = Freq
)
properties = merge(properties, test, by = "tract.number", all.x = T)











#---------------------
#-- Regrouping categorical variables
#---------------------

properties[, airconditioningtypeid := ifelse(airconditioningtypeid %in% c("11", "3", "9"), "1", as.character(airconditioningtypeid))]
properties[, buildingclasstypeid := ifelse(is.na(buildingclasstypeid), 0, 1)]
properties[, heatingorsystemtypeid := ifelse(heatingorsystemtypeid %in% c("1", "10", "11", "12", "13", "14", "18", "20", "19", "21"), 
                               "2", as.character(heatingorsystemtypeid))]
properties[, heatingorsystemtypeid := ifelse(is.na(heatingorsystemtypeid), "-1", as.character(heatingorsystemtypeid))]

#There's 25 different property uses - let's compress them down to 4 categories
properties[, N.prop.type := 
             ifelse(propertylandusetypeid %in% c(260, 261, 262, 263, 264, 265, 266, 267, 268, 270, 271, 273, 275, 276, 279), "Home",
                    ifelse(propertylandusetypeid %in% c(1, 47, 246, 247, 248),  "Mixed",
                           ifelse(propertylandusetypeid %in% c(269, 290, 291 ),  "Not Built", "Other")))]



#---------------------
#-- Altitude
#---------------------
#Please refer to the code 2_altitude_calculation.R

#---------------------
#-- Calculate the distance to the beach
#---------------------
#Please refer to the code 3_EDA_water_distance_calculation.R


water.distance = fread("C:/Quang/Kaggle/Zillow_House_Price/Excel files/water_distance.csv")
properties = merge(properties, water.distance, by.x = "parcelid", by.y = "id.parcel", all.x = T)

properties[, water.distance:= ifelse(is.na(water.distance), mean(water.distance, na.rm = T), water.distance)]

properties[, N.tract.count:= ifelse(is.na(N.tract.count), mean(N.tract.count, na.rm = T), N.tract.count)]
properties[, tract.block:= ifelse(is.na(tract.block), median(tract.block, na.rm = T), tract.block)]
properties[, tract.number:= ifelse(is.na(tract.number), -1, N.tract.count)]

saveRDS(properties, "C:/Quang/Kaggle/Zillow_House_Price_Data/properties_v2.RDS")
