#---------------------
#-- Creating additional variables
#---------------------
# A lot of additional variables can be found here:
#   https://www.kaggle.com/nikunjm88/creating-additional-features




#---------- Features relates to the property
#life of the house
properties[, N.life := 2018 - build.year]

#error of calculation of finished area
properties[, N.living.area.error := area.total.calc / area.live.finished]
properties[, N.living.area.prop := area.total.calc / area.lot]
properties[, N.value.ratio := tax.total / tax.property]
properties[, N.value.prop := tax.building / tax.land]
properties[, N.tax.score := tax.total * tax.property]

test = data.frame(table(properties$region.zip))
test = test %>% rename(
        region.zip = Var1,
        N.zip.count = Freq
)
properties = merge(properties, test, by = "region.zip", all.x = T)

test = data.frame(table(properties$region.city))
test = test %>% rename(
  region.city = Var1,
  N.city.count = Freq
)
properties = merge(properties, test, by = "region.city", all.x = T)


#---------------------
#-- Regrouping categorical variables
#---------------------

properties[, aircon := ifelse(aircon %in% c("11", "3", "9"), "1", as.character(aircon))]
properties[, framing := ifelse(is.na(framing), 0, 1)]
properties[, heating := ifelse(heating %in% c("1", "10", "11", "12", "13", "14", "18", "20", "19", "21"), 
                               "2", as.character(heating))]
properties[, heating := ifelse(is.na(heating), "-1", as.character(heating))]
properties[, zoning.landuse := ifelse(zoning.landuse %in% c("260", "263"))]



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

properties[is.na(properties)] <- -1


