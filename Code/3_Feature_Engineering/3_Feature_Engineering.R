#---------------------
#-- Creating additional variables
#---------------------
# A lot of additional variables can be found here:
#   https://www.kaggle.com/nikunjm88/creating-additional-features




#----------                         Features relates to the property
#----------------------------------------------------------------------

#life of the house
properties = setDT(properties)
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




#======================================
#propertyzoningdesc
#=======================================
properties[, len.use := nchar(as.character(propertyzoningdesc))]
properties[, use.1 := grepl("\\*", propertyzoningdesc)]
properties[, use.2 := grepl("/", propertyzoningdesc)]
properties[, use.3 := grepl("\\d", propertyzoningdesc)]
properties[, use.4 := grepl("-", propertyzoningdesc)]

#---------------------
#-- Regrouping categorical variables
#---------------------

properties[, airconditioningtypeid := ifelse(airconditioningtypeid %in% c("11", "3", "9"), "1", as.character(airconditioningtypeid))]
properties[, buildingclasstypeid := ifelse(is.na(buildingclasstypeid), "-1", as.character(buildingclasstypeid))]
properties[, heatingorsystemtypeid := ifelse(heatingorsystemtypeid %in% c("1", "10", "11", "12", "13", "14", "18", "20", "19", "21"), 
                               "2", as.character(heatingorsystemtypeid))]
properties[, heatingorsystemtypeid := ifelse(is.na(heatingorsystemtypeid), "-1", as.character(heatingorsystemtypeid))]

#There's 25 different property uses - let's compress them down to 4 categories
properties[, N.prop.type := 
             ifelse(propertylandusetypeid %in% c(260, 261, 262, 263, 264, 265, 266, 267, 268, 270, 271, 273, 275, 276, 279), "Home",
                    ifelse(propertylandusetypeid %in% c(1, 47, 246, 247, 248),  "Mixed",
                           ifelse(propertylandusetypeid %in% c(269, 290, 291 ),  "Not Built", "Other")))]

properties[, airconditioningtypeid := ifelse(airconditioningtypeid == "12", "13", as.character(airconditioningtypeid))]
properties[, architecturalstyletypeid := 
             ifelse(architecturalstyletypeid %in% c("1", "2", "3", "4", "5", "6", "10", "27"), 
                    "8", 
                    as.character(architecturalstyletypeid))]

properties <- setDT(data.frame(unclass(properties)))
properties[, buildingqualitytypeid := as.numeric(as.character(buildingqualitytypeid))]
properties[, decktypeid := ifelse(decktypeid == -1, 0, 1)]
properties[, typeconstructiontypeid := ifelse(typeconstructiontypeid %in% c("10", "11"), 
                                              "6",
                                              as.factor(as.character(typeconstructiontypeid)))]
properties[, taxdelinquencyflag := ifelse(taxdelinquencyflag == "", 0, 1)]
summary(properties)

#---------------------
#-- Altitude
#---------------------
#Please refer to the code 2_altitude_calculation.R

#---------------------
#-- Calculate the distance to the beach
#---------------------
#Please refer to the code 3_EDA_water_distance_calculation.R


water.distance = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/water_distance.csv")
properties = merge(properties, water.distance, by.x = "parcelid", by.y = "id.parcel", all.x = T)

properties[, water.distance:= ifelse(is.na(water.distance), median(water.distance, na.rm = T), water.distance)]

saveRDS(properties, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_v2.RDS")
