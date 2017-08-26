#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

#            1. Cleaning

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#


#Run the code number 1, 2, 3 before
path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"
properties = readRDS(paste0(path, "properties_v2.RDS"))

<<<<<<< HEAD
#correct the format of factor variables
=======
properties[, fips := ifelse(is.na(fips), "6037", as.character(fips))]

del.col = c("tract.block", "regionidzip", "regionidcity", "censustractandblock")
properties[ ,(del.col) := NULL]


>>>>>>> 0c2e90abe75ac7e084cd64ebb682e50dcdf459bf

names(properties)[sapply(properties, class) %in% c("factor", "character")]


tract.number

str(properties)
class(properties$tract.block)


