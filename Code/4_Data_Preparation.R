#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

#            1. Cleaning

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#


#Run the code number 1, 2, 3 before
path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"
properties = readRDS(paste0(path, "properties_v2.RDS"))

#correct the format of factor variables

names(properties)[sapply(properties, class) %in% c("factor", "character")]


tract.number

str(properties)
class(properties$tract.block)

