#'''Idea: the beach borders' longitude,altitude can be approximately estimated by the informations
#''' of the most left house for each range of longitude
#the parameter lenght.out here is optimized after several tentatives

#--      1.Using longitue scan
properties$long.percentile = cut(properties$longitude, 
                                 seq(from = min(properties$longitude, na.rm = T), 
                                     to = max(properties$longitude, na.rm = T),
                                     length.out = 200), 
                                 include.lowest=TRUE, 
                                 labels=FALSE)
#Calculate the min in each percentile group
properties[, lat.beach := min(latitude), by = as.factor(long.percentile)]
#check 
test = data.frame(unique(properties[, c("lat.beach", "longitude")]))
qmplot(x = longitude, y = lat.beach, data = test, colour = I('red'), maptype = "watercolor", zoom = 9, margins = T)
#some points are problematic and need to be removed


#--     2.Using latitude scan
properties$lat.percentile = cut(properties$latitude, 
                                seq(from = min(properties$latitude, na.rm = T), 
                                    to = max(properties$latitude, na.rm = T),
                                    length.out = 200), 
                                include.lowest=TRUE, 
                                labels=FALSE)
#Calculate the min in each percentile group
properties[, long.beach := min(longitude), by = as.factor(lat.percentile)]
#check 
test = data.frame(unique(properties[, c("latitude", "long.beach")]))
qmplot(x = long.beach, y = latitude, data = test, colour = I('red'), maptype = "watercolor", zoom = 7, margins = T)



#Combine two approaches
app1 = data.frame(unique(properties[, c("lat.beach", "longitude")]))
app2 = data.frame(unique(properties[, c("latitude", "long.beach")]))

app2 = app2[app2$latitude > 33.783973& app2$latitude < 33.923154, ]
qmplot(x = long.beach, y = latitude, data = app2, colour = I('red'), maptype = "watercolor", zoom = 7, margins = T)
app1 <- app1 %>% rename(
  long.beach = longitude)

app2 <- app2 %>% rename(
  lat.beach = latitude)

test = rbind(app1, app2)
qmplot(x = long.beach, y = lat.beach, data = test, colour = I('red'), maptype = "watercolor", zoom = 7, margins = T)
fwrite(test, "C:/Quang/Kaggle/Zillow_House_Price-master/Zillow_House_Price-master/Graphics/long_lat_of_the_beach.csv")
