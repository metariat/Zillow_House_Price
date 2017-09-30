library(data.table)
library(quantreg)
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

mod201610 = lm(data.merge$logerror ~ 0 + data.merge$me_201610 + data.merge$other_201610)
mod201611 = lm(data.merge$logerror ~ 0 + data.merge$me_201611 + data.merge$other_201611)
mod201612 = lm(data.merge$logerror ~ 0 + data.merge$me_201612 + data.merge$other_201612)
my.coef.10 = abs(mod201610$coefficients[1]) / (abs(mod201610$coefficients[1]) + mod201610$coefficients[2])
my.coef.11 = abs(mod201611$coefficients[1]) / (abs(mod201611$coefficients[1]) + mod201611$coefficients[2])
my.coef.12 = abs(mod201612$coefficients[1]) / (abs(mod201612$coefficients[1]) + mod201612$coefficients[2])



pred.data = merge(me, other, by.x = "parcelid", by.y = "ParcelId", all.x = T) 

x_201610 = round(pred.data$me_201610 * my.coef.10 + pred.data$other_201610 * (1 - my.coef.10), 5)
x_201611 = round(pred.data$me_201611 * my.coef.11 + pred.data$other_201611 * (1 - my.coef.11), 5)
x_201612 = round(pred.data$me_201612 * my.coef.12 + pred.data$other_201612 * (1 - my.coef.12), 5)
x_201710 = round(pred.data$me_201710 * my.coef.10 + pred.data$other_201710 * (1 - my.coef.10), 5)
x_201711 = round(pred.data$me_201711 * my.coef.11 + pred.data$other_201711 * (1 - my.coef.11), 5)
x_201712 = round(pred.data$me_201712 * my.coef.12 + pred.data$other_201712 * (1 - my.coef.12), 5)

subdata = data.frame("parcelid" = pred.data$parcelid, 
                     x_201610, x_201611, x_201612,
                     x_201710, x_201711, x_201712)

setnames(subdata, old = c("x_201610", "x_201611", "x_201612", "x_201710", "x_201711", "x_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))


subdata$parcelid = as.integer((as.character(subdata$parcelid)))
fwrite(subdata, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20170924_ensembling_quantile.csv")
