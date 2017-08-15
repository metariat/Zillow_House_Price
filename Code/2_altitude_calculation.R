library(geonames)
options(geonamesUsername="quang")
library(data.table)


properties = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_2016.csv")

properties <- properties %>% rename(
  id.parcel = parcelid)


GetAl = function(lat, long){
  return(GNsrtm3(lat, long)$srtm3)
}

df1 = properties[1:1000000, ]

df1$altitude = mapply(GetAl, long = df1$longitude, lat = df1$latitude)
fwrite(df1[, c("id.parcel", "altitude")], "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files")

