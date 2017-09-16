library(data.table)
library(stringr)



#---------------------------------------------------------------#

#       Data loading             

#---------------------------------------------------------------#
path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

properties = fread(paste0(path, "properties_v2.csv"), 
                   select = c("parcelid", "rawcensustractandblock"))

demo.data = fread(paste0(path, "external_demo.csv"))



census = unique(properties$rawcensustractandblock)
census = as.character(census)

tract.number = str_sub(census,5,11)
county = str_sub(census,2,4)

demo.data.ini = data.table("tract.number" = tract.number, "county" = county)
demo.data.ini = demo.data.ini[county < 999, ]
demo.data.ini = demo.data.ini[!duplicated(demo.data.ini), ]



#---------------------------------------------------------------#

#       Formatting            

#---------------------------------------------------------------#


#Properties
properties[, census := as.character(rawcensustractandblock)]
properties[, tract.number := str_sub(census,5,11)]
properties[, county := str_sub(census,2,4)]


#-----------    Demo data 
#------------------------------------------------

#income level
table(demo.data$income.level)
demo.data[, income.level := as.character(income.level)]
demo.data[, income.level := ifelse(income.level %in% c("0", "Unwknown", ""), -1,
                            ifelse(income.level == "Low", 1,
                            ifelse(income.level == "Middle", 2,
                            ifelse(income.level == "Moderate", 3, 4))))]


#distressed
table(demo.data$distressed)
demo.data[, distressed := ifelse(distressed == "0", 0, 1)]

#med.income 
demo.data[, med.income := as.numeric(as.character(med.income))]

#y.2016.ffiec.income
demo.data[, y.2016.ffiec.income := gsub(",", "", y.2016.ffiec.income)]
demo.data[, y.2016.ffiec.income := gsub("$", "", y.2016.ffiec.income, fixed = T)]
demo.data[, y.2016.ffiec.income := as.numeric(as.character(y.2016.ffiec.income))]

#y.2016.med.income
demo.data[, y.2016.med.income := gsub(",", "", y.2016.med.income)]
demo.data[, y.2016.med.income := gsub("$", "", y.2016.med.income, fixed = T)]
demo.data[, y.2016.med.income := as.numeric(as.character(y.2016.med.income))]


#y.2010.med.income
demo.data[, y.2010.med.income := gsub(",", "", y.2010.med.income)]
demo.data[, y.2010.med.income := gsub("$", "", y.2010.med.income, fixed = T)]
demo.data[, y.2010.med.income := as.numeric(as.character(y.2010.med.income))]

#tract & county
demo.data$tract.number = demo.data.ini$tract.number
demo.data$county = demo.data.ini$county



#---------------------------------------------------------------#

#       Final treatment           

#---------------------------------------------------------------#


properties = merge(properties, demo.data, by = c("tract.number", "county"), all.x = T)
properties = subset(properties, 
                    select = -c(tract.number, county, rawcensustractandblock, census))

Mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

properties = data.frame(properties)

for(i in 1:ncol(properties)){
  if (class(properties[, i]) == "factor"){
    properties[is.na(properties[,i]), i] <- Mode(properties[, i])
  } 
  else {
    properties[is.na(properties[,i]), i] <- median(properties[, i], na.rm = TRUE)
  }
}


#Feature engineering
names(properties)
properties$income.dev = properties$y.2016.med.income / properties$y.2010.med.income
fwrite(properties, paste0(path, "properties.ext.csv"))
