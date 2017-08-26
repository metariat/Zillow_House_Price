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

#properties[, regionidcity.NA.type := ifelse(is.na(regionidcity ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidcity := ifelse(is.na(regionidcity), "-1",  as.character(regionidcity))]

properties[, calculatedfinishedsquarefeet.NA.type := ifelse(is.na(calculatedfinishedsquarefeet ), -1, 0)] # -1: NA, 0: having value 
properties[, calculatedfinishedsquarefeet := ifelse(is.na(calculatedfinishedsquarefeet), mean(calculatedfinishedsquarefeet, na.rm = TRUE), calculatedfinishedsquarefeet)] #  Calculated total finished living area of the home 

properties[, taxvaluedollarcnt.NA.type := ifelse(is.na(taxvaluedollarcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, taxvaluedollarcnt := ifelse(is.na(taxvaluedollarcnt), mean(taxvaluedollarcnt, na.rm = TRUE), taxvaluedollarcnt)] #  The total tax assessed value of the parcel 

#properties[, regionidzip.NA.typeregionidzip.NA.type := ifelse(is.na(regionidzip ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidzip := ifelse(is.na(regionidzip), "-1", as.character(regionidzip))]

properties[, bathroomcnt.NA.type := ifelse(is.na(bathroomcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, bathroomcnt := ifelse(is.na(bathroomcnt), median(bathroomcnt, na.rm = TRUE), bathroomcnt)] #   Number of bathrooms in home including fractional bathrooms

properties[, assessmentyear.NA.type := ifelse(is.na(assessmentyear ), -1, 0)] # -1: NA, 0: having value 
properties[, assessmentyear := ifelse(is.na(assessmentyear), median(assessmentyear, na.rm = TRUE), assessmentyear)] #   tax year

#properties[, rawcensustractandblock.NA.type := ifelse(is.na(rawcensustractandblock ), -1, 0)] # -1: NA, 0: having value 
properties[, rawcensustractandblock := ifelse(is.na(rawcensustractandblock), "999999999", as.character(rawcensustractandblock))]

properties[, regionidcounty.NA.type := ifelse(is.na(regionidcounty ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidcounty := ifelse(is.na(regionidcounty), "-1",  as.character(regionidcounty))]

properties[, longitude.NA.type := ifelse(is.na(longitude ), -1, 0)] # -1: NA, 0: having value 
properties[, longitude := ifelse(is.na(longitude), mean(longitude, na.rm = TRUE), longitude)] #  longtitude

#properties[, fips.NA.type := ifelse(is.na(fips ), -1, 0)] # -1: NA, 0: having value 
#properties[, fips := ifelse(is.na(fips), "-1", as.character(fips))]

#properties[, censustractandblock.NA.type := ifelse(is.na(censustractandblock ), -1, 0)] # -1: NA, 0: having value 
#properties[, censustractandblock := ifelse(is.na(censustractandblock), mean(censustractandblock, na.rm = TRUE), censustractandblock)] 

#properties[, propertycountylandusecode.NA.type := ifelse(is.na(propertycountylandusecode ), -1, 0)] # -1: NA, 0: having value 
properties[, propertycountylandusecode := ifelse(is.na(propertycountylandusecode), "-1", as.character(propertycountylandusecode))]

properties[, poolcnt.NA.type := ifelse(is.na( poolcnt ), -1, 0)] # -1: NA, 0: having value 
properties[,  poolcnt := ifelse(is.na( poolcnt), median( poolcnt, na.rm = TRUE),  poolcnt)] #   Number of pools on the lot (if any)






####### Edit from Tuan Anh 

#census: created from 'rawcensustractandblock'(Census tract and block ID combined - also contains blockgroup assignment by extension)
class(properties$census)
summary(properties$census)
sum(is.na(properties$census))

properties[, census := ifelse(is.na(properties$census), "-1", properties$census)]

#latitude
#class(properties$latitude) #integer
#summary(properties$latitude) #11437 NA
properties[, latitude.NA.type := ifelse(is.na(latitude ), -1, 0)] # -1: NA, 0: having value 
properties[, latitude := ifelse(is.na(latitude), mean(latitude, na.rm = TRUE), latitude)]

#'propertylandusetypeid' : Type of land use the property is zoned for 
class(properties$propertylandusetypeid) #integer
summary(properties$propertylandusetypeid) #11437 NA
#properties[, propertylandusetypeid.NA.type := ifelse(is.na(propertylandusetypeid), -1, 0)] # -1: NA, 0: having value 
properties[, propertylandusetypeid := ifelse(is.na(propertylandusetypeid), "261", propertylandusetypeid)]

#tract.block: created from 'rawcensustractandblock'
class(properties$tract.block) #character
summary(properties$tract.block) 
#properties[, tract.block.NA.type := ifelse(is.na(tract.block ), -1, 0)] # -1: NA, 0: having value 
#properties[, tract.block := ifelse(is.na(tract.block), "999999999", as.character(tract.block))]

#tract.number: created from 'rawcensustractandblock'
class(properties$tract.number) #character
summary(properties$tract.number) 
#properties[, tract.number.NA.type := ifelse(is.na(tract.number), -1, 0)] # -1: NA, 0: having value 
#properties[, tract.number := ifelse(is.na(tract.number), "999999999", as.character(tract.number))]



#taxamount: The total property tax assessed for that assessment year => NA = mean
class(properties$taxamount) #numeric
summary(properties$taxamount) #31250 NA
#properties[, taxamount.NA.type := ifelse(is.na(taxamount), -1, 0)] # -1: NA, 0: having value 
properties[, taxamount := ifelse(is.na(taxamount), mean(taxamount, na.rm = TRUE), taxamount)]

#'structuretaxvaluedollarcnt': The assessed value of the built structure on the parcel => NA = mean
class(properties$structuretaxvaluedollarcnt) #numeric
summary(properties$structuretaxvaluedollarcnt) #54982 NA
#properties[, structuretaxvaluedollarcnt.NA.type := ifelse(is.na(structuretaxvaluedollarcnt), -1, 0)] # -1: NA, 0: having value 
properties[, structuretaxvaluedollarcnt := ifelse(is.na(structuretaxvaluedollarcnt), mean(structuretaxvaluedollarcnt, na.rm = TRUE), structuretaxvaluedollarcnt)]


#landtaxvaluedollarcnt: The assessed value of the land area of the parcel => NA = mean
class(properties$landtaxvaluedollarcnt) #numeric
summary(properties$landtaxvaluedollarcnt) #67733 NA
#properties[, landtaxvaluedollarcnt.NA.type := ifelse(is.na(landtaxvaluedollarcnt), -1, 0)] # -1: NA, 0: having value 
properties[, landtaxvaluedollarcnt := ifelse(is.na(landtaxvaluedollarcnt), mean(landtaxvaluedollarcnt, na.rm = TRUE), landtaxvaluedollarcnt)]

#yearbuilt > NA = median (1963)
class(properties$yearbuilt) #numeric
summary(properties$yearbuilt) #59928 NA
#properties[, yearbuilt.NA.type := ifelse(is.na(yearbuilt), -1, 0)] # -1: NA, 0: having value 
properties[, yearbuilt := ifelse(is.na(yearbuilt), median(yearbuilt, na.rm = TRUE), yearbuilt)]

#calculatedbathnbr:  Number of bathrooms in home including fractional bathroom => NA = median (=2)
class(properties$calculatedbathnbr) #numeric
summary(properties$calculatedbathnbr) #128912 NA
properties[, calculatedbathnbr.NA.type := ifelse(is.na(calculatedbathnbr), -1, 0)] # -1: NA, 0: having value 
properties[, calculatedbathnbr := ifelse(is.na(calculatedbathnbr), median(calculatedbathnbr, na.rm = TRUE), calculatedbathnbr)]


#bedroom_cnt:  Number of bedrooms in home =>NA = median (=3)
class(properties$bedroomcnt) #numeric
summary(properties$bedroomcnt)#11450 NA
properties[, bedroomcnt.NA.type := ifelse(is.na(bedroomcnt), -1, 0)] # -1: NA, 0: having value 
properties[, bedroomcnt := ifelse(is.na(bedroomcnt), median(bedroomcnt, na.rm = TRUE), bedroomcnt)]

#roomcnt:   Total number of rooms in the principal residence =>NA = median (=0)
class(properties$roomcnt) #numeric
summary(properties$roomcnt) #11475 NA
properties[, roomcnt.NA.type := ifelse(is.na(roomcnt), -1, 0)] # -1: NA, 0: having value 
properties[, roomcnt := ifelse(is.na(roomcnt), median(roomcnt, na.rm = TRUE), roomcnt)]


#lotsizesquarefeet:    Area of the lot in square feet
class(properties$lotsizesquarefeet) #numeric
summary(properties$lotsizesquarefeet) #276099 NA
properties[, lotsizesquarefeet.NA.type := ifelse(is.na(lotsizesquarefeet), -1, 0)] # -1: NA, 0: having value 
properties[, lotsizesquarefeet := ifelse(is.na(lotsizesquarefeet), median(lotsizesquarefeet, na.rm = TRUE), lotsizesquarefeet)]

#buildingqualitytypeid :  Overall assessment of condition of the building from best (lowest) to worst (highest) => median
class(properties$buildingqualitytypeid) #integer
summary(properties$buildingqualitytypeid) #11437 NA
table(properties$buildingqualitytypeid) #11437 NA
#properties[, buildingqualitytypeid.NA.type := ifelse(is.na(buildingqualitytypeid ), -1, 0)] # -1: NA, 0: having value 
properties[, buildingqualitytypeid := ifelse(is.na(buildingqualitytypeid), 7, buildingqualitytypeid)]


#regionidneighborhood: Neighborhood in which the property is located => NA = -1
class(properties$regionidneighborhood) #integer
summary(properties$regionidneighborhood) #1828815 NA
#properties[, regionidneighborhood.NA.type := ifelse(is.na(regionidneighborhood ), -1, 0)] # -1: NA, 0: having value 
properties[, regionidneighborhood := ifelse(is.na(regionidneighborhood), "-1", regionidneighborhood)]


#'garagecarcnt': Total number of garages on the lot including an attached garage
class(properties$garagecarcnt) #integer
summary(properties$garagecarcnt) #2101950 NA
#properties[, garagecarcnt.NA.type := ifelse(is.na(garagecarcnt ), -1, 0)] # -1: NA, 0: having value 
properties[, garagecarcnt := ifelse(is.na(garagecarcnt), median(garagecarcnt, na.rm = TRUE), garagecarcnt)]

#'finishedfloor1squarefeet':  Size of the finished living area on the first (entry) floor of the home => NA = mean
class(properties$finishedfloor1squarefeet) #integer
summary(properties$finishedfloor1squarefeet) #2782500 NA
properties[, finishedfloor1squarefeet.NA.type := ifelse(is.na(finishedfloor1squarefeet ), -1, 0)] # -1: NA, 0: having value 
properties[, finishedfloor1squarefeet := ifelse(is.na(finishedfloor1squarefeet), -1 , finishedfloor1squarefeet)]


#'finishedsquarefeet15':  Total area => NA = mean
class(properties$finishedsquarefeet15) #integer
summary(properties$finishedsquarefeet15) #2794419 NA
properties[, finishedsquarefeet15.NA.type := ifelse(is.na(finishedsquarefeet15 ), -1, 0)] # -1: NA, 0: having value 
properties[, finishedsquarefeet15 := ifelse(is.na(finishedsquarefeet15), median(finishedsquarefeet15, na.rm = TRUE), finishedsquarefeet15)] #  Size of the finished living area on the first (entry) floor of the home

#'finishedsquarefeet6':  Base unfinished and finished area => NA = mean
class(properties$finishedsquarefeet6) #integer
summary(properties$finishedsquarefeet6) #2963216 NA
properties[, finishedsquarefeet6.NA.type := ifelse(is.na(finishedsquarefeet6 ), -1, 0)] # -1: NA, 0: having value 
properties[, finishedsquarefeet6 := ifelse(is.na(finishedsquarefeet6), median(finishedsquarefeet6, na.rm = TRUE), finishedsquarefeet6)] 

#'finishedsquarefeet13':  Perimeter  living area
class(properties$finishedsquarefeet13) #integer
summary(properties$finishedsquarefeet13) #2963216 NA
properties[, finishedsquarefeet13.NA.type := ifelse(is.na(finishedsquarefeet13 ), -1, 0)] # -1: NA, 0: having value 
properties[, finishedsquarefeet13 := ifelse(is.na(finishedsquarefeet13), median(finishedsquarefeet13, na.rm = TRUE), finishedsquarefeet6)] 


#taxdelinquencyyear:    Year for which the unpaid propert taxes were due => NA = median (14 years) 
class(properties$taxdelinquencyyear) #integer
summary(properties$taxdelinquencyyear) #2928753 NA
properties[, taxdelinquencyyear.NA.type := ifelse(is.na(taxdelinquencyyear), -1, 0)] # -1: NA, 0: having value 
properties[, taxdelinquencyyear := ifelse(is.na(taxdelinquencyyear), median(taxdelinquencyyear, na.rm = TRUE), taxdelinquencyyear)]


#typeconstructiontypeid:     What type of construction material was used to construct the home (1 - 18) => NA = -1
class(properties$typeconstructiontypeid) #integer
summary(properties$typeconstructiontypeid) #2928753 NA
#properties[, typeconstructiontypeid.NA.type := ifelse(is.na(typeconstructiontypeid), -1, 0)] # -1: NA, 0: having value 
properties[, typeconstructiontypeid := ifelse(is.na(typeconstructiontypeid), "-1", typeconstructiontypeid)]

#architecturalstyletypeid:     Architectural style of the home (i.e. ranch, colonial, split-level, etc…) => NA = -1 
class(properties$architecturalstyletypeid) #integer
summary(properties$architecturalstyletypeid) #2979156 NA
#properties[, architecturalstyletypeid.NA.type := ifelse(is.na(architecturalstyletypeid), -1, 0)] # -1: NA, 0: having value 
properties[, architecturalstyletypeid := ifelse(is.na(architecturalstyletypeid), "-1", architecturalstyletypeid)]


#basementsqft  Finished living area below or partially below ground level => N/A = mean
class(properties$basementsqft) #integer
summary(properties$basementsqft) #2979156 NA
properties[, basementsqft.NA.type := ifelse(is.na(basementsqft), -1, 0)] # -1: NA, 0: having value 
properties[, basementsqft := ifelse(is.na(basementsqft), mean(basementsqft, na.rm = TRUE), basementsqft)]


properties[, pooltypeid2 := ifelse(is.na(pooltypeid2), "-1", as.character(pooltypeid2))]
properties[, pooltypeid7 := ifelse(is.na(pooltypeid7), "-1", as.character(pooltypeid7))]



