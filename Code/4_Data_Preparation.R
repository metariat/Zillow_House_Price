#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

#            1. Cleaning

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#


#Run the code number 1, 2, 3 before
properties = readRDS("C:/Quang/Kaggle/Zillow_House_Price_Data/properties_v2.RDS")


missing.values <- transpose(data.frame(sapply(properties, function(x) sum(is.na(x))/length(x))))
colnames(missing.values) = colnames(properties)

missing.values <- gather(missing.values, key="feature", value="missing_pct")
missing.values %>% 
  ggplot(aes(x=reorder(feature,missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red") +
  coord_flip()+theme_bw()


