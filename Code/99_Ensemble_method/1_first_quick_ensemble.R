library(data.table)
other = fread("C:/Users/xq.do/Downloads/sub20170821_221910.csv", header = T)
me = fread("C:/Users/xq.do/Downloads/20170909_Q_Quick_tuned_catboost_with_month_v2.csv", header = T)

path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path, "train.RDS")))
test = setDT(readRDS(paste0(path, "test.RDS")))

setnames(me, old = c("201610", "201611", "201612", "201710", "201711", "201712"),
         new = c("me_201610", "me_201611", "me_201612", "me_201710", "me_201711", "me_201712"))

setnames(other, old = c("201610", "201611", "201612", "201710", "201711", "201712"),
         new = c("other_201610", "other_201611", "other_201612", "other_201710", "other_201711", "other_201712"))

data = rbind(train, test)


#-----------------------------------------------#
data.merge = merge(data, me, by = "parcelid", all.x = T)
data.merge = merge(data.merge, other, by.x = "parcelid", by.y = "ParcelId", all.x = T)

pred.data = merge(me, other, by.x = "parcelid", by.y = "ParcelId", all.x = T) 

my.coef = 0.2962963
other.coef = 1 - my.coef

x_201610 = round(pred.data$me_201610 * my.coef + pred.data$other_201610 * other.coef, 5)
x_201611 = round(pred.data$me_201611 * my.coef + pred.data$other_201611 * other.coef, 5)
x_201612 = round(pred.data$me_201612 * my.coef + pred.data$other_201612 * other.coef, 5)
x_201710 = round(pred.data$me_201710 * my.coef + pred.data$other_201710 * other.coef, 5)
x_201711 = round(pred.data$me_201711 * my.coef + pred.data$other_201711 * other.coef, 5)
x_201712 = round(pred.data$me_201712 * my.coef + pred.data$other_201712 * other.coef, 5)

subdata = data.frame("parcelid" = pred.data$parcelid, 
                     x_201610, x_201611, x_201612,
                     x_201710, x_201711, x_201712)

setnames(subdata, old = c("x_201610", "x_201611", "x_201612", "x_201710", "x_201711", "x_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))


subdata$parcelid = as.integer((as.character(subdata$parcelid)))
fwrite(subdata, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170924_ensembling.csv")
