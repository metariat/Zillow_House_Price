#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#

# ---                       DATA IMPUTATION AND FORMATING                             --- #

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#


#Transforming bad formatting categorical variables
properties$fireplaceflag = ifelse(properties$fireplaceflag == "true", 1, 0)
properties[, pooltypeid2 := 
               ifelse(pooltypeid2 == 1 & is.na(pooltypeid2) == F, 
                      1, 
                      ifelse(poolcnt == 1 & is.na(poolcnt) == F,0, -1))]

properties[, pooltypeid7 := ifelse(pooltypeid7 == 1 & is.na(pooltypeid7) == F, 
                                   1, 
                                   ifelse(poolcnt == 1 & is.na(poolcnt) == F, 0, -1))]

properties[, pooltypeid2 := as.factor(as.character(pooltypeid2))]
properties[, pooltypeid7 := as.factor(as.character(pooltypeid7))]

properties[, pooltypeid10 := ifelse(pooltypeid10 == 1 & is.na(pooltypeid10) == F, 1, 0)]

properties[, hashottuborspa := ifelse(hashottuborspa == "true", 1, 0)]

properties <- properties %>% 
  mutate(census = as.character(rawcensustractandblock), 
         tract.number = str_sub(census,5,11), 
         tract.block = str_sub(census,12))


#Some varialbes in which missing value is not really missing

properties = setDT(properties) 
properties[, airconditioningtypeid := ifelse(is.na(airconditioningtypeid), "-1", 
                                             as.character(airconditioningtypeid))]

properties[, fireplacecnt := ifelse(is.na(fireplacecnt), 0, fireplacecnt)]

properties[, poolcnt := ifelse(is.na(poolcnt), 0, poolcnt)]

properties[, is.city := ifelse(is.na(regionidcity), 0, 1)]

properties[, decktypeid := ifelse(is.na(decktypeid), "-1", as.character(decktypeid))]

properties[, storytypeid := ifelse(is.na(storytypeid), 0, 1)]

properties[, censustractandblock := as.numeric(as.character(censustractandblock))]
str(properties)

properties <- data.frame(unclass(properties))


Mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in 1:ncol(properties)){
  if (class(properties[, i]) == "factor"){
    properties[is.na(properties[,i]), i] <- Mode(properties[, i])
  } 
  else {
    properties[is.na(properties[,i]), i] <- median(properties[, i], na.rm = TRUE)
  }
}