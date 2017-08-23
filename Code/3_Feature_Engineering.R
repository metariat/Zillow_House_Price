#---------------------
#-- Creating additional variables
#---------------------
# A lot of additional variables can be found here:
#   https://www.kaggle.com/nikunjm88/creating-additional-features




#---------- Features relates to the property
#life of the house
properties[, N.life := 2018 - yearbuilt]

#error of calculation of finished area
properties[, N.living.area.error := calculatedfinishedsquarefeet / finishedsquarefeet12]
properties[, N.living.area.prop := calculatedfinishedsquarefeet / lotsizesquarefeet]
properties[, N.value.ratio := taxvaluedollarcnt / taxamount]
properties[, N.value.prop := structuretaxvaluedollarcnt / landtaxvaluedollarcnt]
properties[, N.tax.score := taxvaluedollarcnt * taxamount]

test = data.frame(table(properties$regionidzip))
test = test %>% rename(
        regionidzip = Var1,
        N.zip.count = Freq
)
properties = merge(properties, test, by = "regionidzip", all.x = T)

test = data.frame(table(properties$regionidcity))
test = test %>% rename(
  regionidcity = Var1,
  N.city.count = Freq
)
properties = merge(properties, test, by = "regionidcity", all.x = T)


#---------------------
#-- Regrouping categorical variables
#---------------------

properties[, airconditioningtypeid := ifelse(airconditioningtypeid %in% c("11", "3", "9"), "1", as.character(airconditioningtypeid))]
properties[, buildingclasstypeid := ifelse(is.na(buildingclasstypeid), 0, 1)]
properties[, heatingorsystemtypeid := ifelse(heatingorsystemtypeid %in% c("1", "10", "11", "12", "13", "14", "18", "20", "19", "21"), 
                               "2", as.character(heatingorsystemtypeid))]
properties[, heatingorsystemtypeid := ifelse(is.na(heatingorsystemtypeid), "-1", as.character(heatingorsystemtypeid))]
properties[, propertylandusetypeid := ifelse(propertylandusetypeid %in% c("260", "263"))]



#---------------------
#-- Altitude
#---------------------
#Please refer to the code 2_altitude_calculation.R

#---------------------
#-- Calculate the distance to the beach
#---------------------
#Please refer to the code 3_EDA_water_distance_calculation.R


water.distance = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/water_distance.csv")
properties = merge(properties, water.distance, by = "id.parcel", all.x = T)


