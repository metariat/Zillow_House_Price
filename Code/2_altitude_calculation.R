library(geonames)
options(geonamesUsername="quang")
library(data.table)


properties = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_2016.csv")

properties <- properties %>% rename(
  id.parcel = parcelid)


GetAl = function(lat, long){
  return(GNsrtm3(lat, long)$srtm3)
}

df1 = properties[1:500000, ]
df1$altitude = mapply(GetAl, long = df1$longitude, lat = df1$latitude)
fwrite(df1[, c("id.parcel", "altitude")], 
       "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/altitude_df1.csv")

df2 = properties[1000001:2000000, ]
df2$altitude = mapply(GetAl, long = df2$longitude, lat = df2$latitude)
fwrite(df2[, c("id.parcel", "altitude")], 
       "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/altitude_df2.csv")

df3 = properties[2000001:nrow(properties), ]
df3$altitude = mapply(GetAl, long = df3$longitude, lat = df3$latitude)
fwrite(df3[, c("id.parcel", "altitude")], 
       "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/altitude_df3.csv")


