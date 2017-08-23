#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#

# ---                       DATA IMPUTATION AND FORMATING                             --- #

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#


#Transforming bad formatting categorical variables
properties$fireplaceflag = ifelse(properties$fireplaceflag == "true", 1, 0)
properties[, pooltypeid2 := ifelse(pooltypeid2 == 1, 1, 
                                   ifelse(poolcnt == 1,0, -1))]
properties[, pooltypeid7 := ifelse(pooltypeid7 == 1, 1, 
                                   ifelse(poolcnt ==1, 0, -1))]
properties[, pooltypeid10 := ifelse(pooltypeid10 == 1, 1, 0)]
properties[, hashottuborspa := ifelse(hashottuborspa == "true", 1, 0)]
properties <- properties %>% 
  mutate(census = as.character(rawcensustractandblock), 
         tract.number = str_sub(census,5,11), 
         tract.block = str_sub(census,12))


#Some varialbes in which missing value is not really missing

properties = setDT(properties) 
properties[, airconditioningtypeid := ifelse(is.na(airconditioningtypeid), "-1", 
                                             as.character(airconditioningtypeid))]

properties[, is.fire.place := ifelse(is.na(fireplacecnt), 0, 1)]

properties[, fireplacecnt := ifelse(is.na(fireplacecnt), 0, fireplacecnt)]

properties[, poolcnt := ifelse(is.na(poolcnt), 0, poolcnt)]

properties[, is.city := ifelse(is.na(regionidcity), 0, 1)]

properties[, decktypeid := ifelse(is.na(decktypeid), "-1", as.character(decktypeid))]

properties[, storytypeid := ifelse(is.na(storytypeid), "-1", as.character(storytypeid))] #type of house (if contain "sous-sol", "grenier", etc.)

properties[, yardbuildingsqft26.NA.type := ifelse(is.na(yardbuildingsqft26), -1, 0)] # -1: NA, 0: having value 
properties[, yardbuildingsqft26 := ifelse(is.na(yardbuildingsqft26), mean(yardbuildingsqft26, na.rm = TRUE), yardbuildingsqft26)] #surface of the storage in the yard

properties[, yardbuildingsqft17.NA.type := ifelse(is.na(yardbuildingsqft17 ), -1, 0)] # -1: NA, 0: having value 
properties[, yardbuildingsqft17 := ifelse(is.na(yardbuildingsqft17), mean(yardbuildingsqft17, na.rm = TRUE), yardbuildingsqft17)] #surface of the patio in the yard

properties[, heatingorsystemtypeid := ifelse(is.na(heatingorsystemtypeid), "-1", as.character(heatingorsystemtypeid))]

properties[, buildingclasstypeid := ifelse(is.na(buildingclasstypeid), "-1", as.character(buildingclasstypeid))]

properties[, poolsizesum.NA.type := ifelse(is.na(poolsizesum ), -1, 0)] # -1: NA, 0: having value 
properties[, poolsizesum := ifelse(is.na(poolsizesum), mean(poolsizesum, na.rm = TRUE), poolsizesum)] # Total square footage of all pools on property

properties[, pooltypeid10 := ifelse(is.na(pooltypeid10), "-1", as.character(pooltypeid10))]

properties[, finishedsquarefeet50.NA.type := ifelse(is.na(finishedsquarefeet50 ), -1, 0)] # -1: NA, 0: having value 
properties[, finishedsquarefeet50 := ifelse(is.na(finishedsquarefeet50), mean(finishedsquarefeet50, na.rm = TRUE), finishedsquarefeet50)] #  Size of the finished living area on the first (entry) floor of the home

properties[, threequarterbathnbr.NA.type := ifelse(is.na(threequarterbathnbr ), -1, 0)] # -1: NA, 0: having value 
properties[, threequarterbathnbr := ifelse(is.na(threequarterbathnbr), mean(threequarterbathnbr, na.rm = TRUE), threequarterbathnbr)]#Number of 3/4 bathrooms in house (shower + sink + toilet)

properties[, numberofstories.NA.type := ifelse(is.na(numberofstories ), -1, 0)] # -1: NA, 0: having value 
properties[, numberofstories := ifelse(is.na(numberofstories), median(numberofstories, na.rm = TRUE), numberofstories)]# Number of stories or levels the home has

properties[, garagetotalsqft.NA.type := ifelse(is.na(garagetotalsqft ), -1, 0)] # -1: NA, 0: having value 
properties[, garagetotalsqft := ifelse(is.na(garagetotalsqft), mean(garagetotalsqft, na.rm = TRUE), garagetotalsqft)] # Total square garage

properties[, heatingorsystemtypeid := ifelse(is.na(heatingorsystemtypeid), "-1", as.character(heatingorsystemtypeid))]

properties[, unitcnt.NA.type := ifelse(is.na(unitcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, unitcnt := ifelse(is.na(unitcnt), median(unitcnt, na.rm = TRUE), unitcnt)] #  Number of units the structure is built into (i.e. 2 = duplex, 3 = triplex, etc...)

properties[, finishedsquarefeet12.NA.type := ifelse(is.na(finishedsquarefeet12 ), -1, 0)] # -1: NA, 0: having value 
properties[, finishedsquarefeet12 := ifelse(is.na(finishedsquarefeet12), mean(finishedsquarefeet12, na.rm = TRUE), finishedsquarefeet12)] #  Finished living area

properties[, fullbathcnt.NA.type := ifelse(is.na(fullbathcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, fullbathcnt := ifelse(is.na(fullbathcnt), median(fullbathcnt, na.rm = TRUE), fullbathcnt)] #  Number of full bathrooms (sink, shower + bathtub, and toilet) present in home

properties[, regionidcity.NA.type := ifelse(is.na(regionidcity ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidcity := ifelse(is.na(regionidcity), "-1",  as.character(regionidcity))]

properties[, calculatedfinishedsquarefeet.NA.type := ifelse(is.na(calculatedfinishedsquarefeet ), -1, 0)] # -1: NA, 0: having value 
properties[, calculatedfinishedsquarefeet := ifelse(is.na(calculatedfinishedsquarefeet), mean(calculatedfinishedsquarefeet, na.rm = TRUE), calculatedfinishedsquarefeet)] #  Calculated total finished living area of the home 

properties[, taxvaluedollarcnt.NA.type := ifelse(is.na(taxvaluedollarcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, taxvaluedollarcnt := ifelse(is.na(taxvaluedollarcnt), mean(taxvaluedollarcnt, na.rm = TRUE), taxvaluedollarcnt)] #  The total tax assessed value of the parcel 

properties[, regionidzip.NA.typeregionidzip.NA.type := ifelse(is.na(regionidzip ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidzip := ifelse(is.na(regionidzip), "-1", as.character(regionidzip))]

properties[, bathroomcnt.NA.type := ifelse(is.na(bathroomcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, bathroomcnt := ifelse(is.na(bathroomcnt), median(bathroomcnt, na.rm = TRUE), bathroomcnt)] #   Number of bathrooms in home including fractional bathrooms

properties[, assessmentyear.NA.type := ifelse(is.na(assessmentyear ), -1, 0)] # -1: NA, 0: having value 
properties[, assessmentyear := ifelse(is.na(assessmentyear), median(assessmentyear, na.rm = TRUE), assessmentyear)] #   tax year

properties[, rawcensustractandblock.NA.type := ifelse(is.na(rawcensustractandblock ), -1, 0)] # -1: NA, 0: having value 
properties[, rawcensustractandblock := ifelse(is.na(rawcensustractandblock), "999999999", as.character(rawcensustractandblock))]

properties[, regionidcounty.NA.type := ifelse(is.na(regionidcounty ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidcounty := ifelse(is.na(regionidcounty), "-1",  as.character(regionidcounty))]

properties[, longitude.NA.type := ifelse(is.na(longitude ), -1, 0)] # -1: NA, 0: having value 
properties[, longitude := ifelse(is.na(longitude), mean(longitude, na.rm = TRUE), longitude)] #  longtitude

properties[, fips.NA.type := ifelse(is.na(fips ), -1, 0)] # -1: NA, 0: having value 
properties[, fips := ifelse(is.na(fips), "-1", as.character(fips))]

properties[, censustractandblock.NA.type := ifelse(is.na(censustractandblock ), -1, 0)] # -1: NA, 0: having value 
properties[, censustractandblock := ifelse(is.na(censustractandblock), mean(censustractandblock, na.rm = TRUE), censustractandblock)] 

properties[, propertycountylandusecode.NA.type := ifelse(is.na(propertycountylandusecode ), -1, 0)] # -1: NA, 0: having value 
properties[, propertycountylandusecode := ifelse(is.na(propertycountylandusecode), "-1", as.character(propertycountylandusecode))]

properties[, poolcnt.NA.type := ifelse(is.na( poolcnt ), -1, 0)] # -1: NA, 0: having value 
properties[,  poolcnt := ifelse(is.na( poolcnt), median( poolcnt, na.rm = TRUE),  poolcnt)] #   Number of pools on the lot (if any)

