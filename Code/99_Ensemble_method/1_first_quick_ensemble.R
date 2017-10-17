library(data.table)
library(quantreg)
other = fread("C:/Users/xq.do/Downloads/sub20171010_190123.csv", header = T)
me1 = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20171007_Q_catboost_python_water_density_fe_month_5x_500_trees_v2.csv", header = T)
me2 = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20171007_Q_catboost_python_water_density_fe_month_15x_600_trees_v2.csv", header = T)
me3 = fread("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20171007_Q_catboost_python_water_density_fe_month_20x_600_trees_v2.csv", header = T)

me = copy(me1)
me$'201610' = (me1$'201610' * 5 + me2$'201610' * 15 + me3$'201610' * 20) / 40
me$'201611' = (me1$'201611' * 5 + me2$'201611' * 15 + me3$'201611' * 20) / 40
me$'201612' = (me1$'201612' * 5 + me2$'201612' * 15 + me3$'201612' * 20) / 40
me$'201710' = (me1$'201710' * 5 + me2$'201710' * 15 + me3$'201710' * 20) / 40
me$'201711' = (me1$'201711' * 5 + me2$'201711' * 15 + me3$'201711' * 20) / 40
me$'201712' = (me1$'201712' * 5 + me2$'201712' * 15 + me3$'201712' * 20) / 40


path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train = setDT(readRDS(paste0(path, "train.RDS")))
test = setDT(readRDS(paste0(path, "test.RDS")))

setnames(me, old = c("201610", "201611", "201612", "201710", "201711", "201712"),
         new = c("me_201610", "me_201611", "me_201612", "me_201710", "me_201711", "me_201712"))

setnames(other, old = c("201610", "201611", "201612", "201710", "201711", "201712"),
         new = c("other_201610", "other_201611", "other_201612", "other_201710", "other_201711", "other_201712"))

data = rbind(train, test)


#-----------------------------------------------#
data.merge = merge(data, me, by.x = "parcelid", by.y = "ParcelId", all.x = T)
data.merge = merge(data.merge, other, by.x = "parcelid", by.y = "ParcelId", all.x = T)

mod201610 = rq(data.merge$logerror ~ 0 + data.merge$me_201610 + data.merge$other_201610)
mod201611 = rq(data.merge$logerror ~ 0 + data.merge$me_201611 + data.merge$other_201611)
mod201612 = rq(data.merge$logerror ~ 0 + data.merge$me_201612 + data.merge$other_201612)
co = 0.8
my.coef.10 = abs(mod201610$coefficients[1]) / (abs(mod201610$coefficients[1]) + abs(mod201610$coefficients[2])) * co
my.coef.11 = abs(mod201611$coefficients[1]) / (abs(mod201611$coefficients[1]) + abs(mod201611$coefficients[2])) * co
my.coef.12 = abs(mod201612$coefficients[1]) / (abs(mod201612$coefficients[1]) + abs(mod201612$coefficients[2])) * co



pred.data = merge(me, other, by.x = "ParcelId", by.y = "ParcelId", all.x = T) 

x_201610 = round(pred.data$me_201610 * my.coef.10 + pred.data$other_201610 * (1 - my.coef.10), 5)
x_201611 = round(pred.data$me_201611 * my.coef.11 + pred.data$other_201611 * (1 - my.coef.11), 5)
x_201612 = round(pred.data$me_201612 * my.coef.12 + pred.data$other_201612 * (1 - my.coef.12), 5)
x_201710 = round(pred.data$me_201710 * my.coef.10 + pred.data$other_201710 * (1 - my.coef.10), 5)
x_201711 = round(pred.data$me_201711 * my.coef.11 + pred.data$other_201711 * (1 - my.coef.11), 5)
x_201712 = round(pred.data$me_201712 * my.coef.12 + pred.data$other_201712 * (1 - my.coef.12), 5)

subdata = data.frame("parcelid" = pred.data$ParcelId, 
                     x_201610, x_201611, x_201612,
                     x_201710, x_201711, x_201712)

setnames(subdata, old = c("x_201610", "x_201611", "x_201612", "x_201710", "x_201711", "x_201712"),
         new = c("201610", "201611", "201612", "201710", "201711", "201712"))


subdata$parcelid = as.integer((as.character(subdata$parcelid)))
fwrite(subdata, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/submission/20171002_ensembling_catboost_with_fe_600trees_and_others.csv")
