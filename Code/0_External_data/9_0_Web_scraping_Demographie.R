# install.packages("rvest")
library(rvest)
library(data.table)
library(stringr) #regrex
library(curl)


#---------------------------------------------------------#

#                      Data loading                       #

#---------------------------------------------------------#

path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

properties = fread(paste0(path, "properties_v2.csv"), select = c("rawcensustractandblock"))
census = unique(properties$rawcensustractandblock)
census = as.character(census)

tract.number = str_sub(census,5,11)
county = str_sub(census,2,4)

demo.data = data.table("tract.number" = tract.number, "county" = county)
demo.data = demo.data[county < 999, ]
demo.data = demo.data[!duplicated(demo.data), ]
rm(properties)
gc()

income.level 				= rep(-1,nrow(demo.data))
distressed         	= rep(-1,nrow(demo.data))
med.income 	        = rep(-1,nrow(demo.data))
y.2016.ffiec.income = rep(-1,nrow(demo.data))
y.2016.med.income 	= rep(-1,nrow(demo.data))
y.2010.med.income 	= rep(-1,nrow(demo.data))
population 	        = rep(-1,nrow(demo.data))
minority.percentage = rep(-1,nrow(demo.data))
minority.pop 	      = rep(-1,nrow(demo.data))
owner.occupied    	= rep(-1,nrow(demo.data))
minor.family.unit 	= rep(-1,nrow(demo.data))



time1 = Sys.time()
for (i in 2353:1){
  Sys.sleep(5)
  county = demo.data$county[i]
  tract = demo.data$tract.number[i]
  url1 = "https://www.ffiec.gov/census/report.aspx?year=2016&county="
  url2 = "&tract="
  url3 = "&state=06&report=demographic"
  url = paste0(url1, county, url2, tract, url3)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  webpage <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  if (!is.na(html_text(html_nodes(webpage,'td:nth-child(1)'))[14])){
    income.level[i]        = html_text(html_nodes(webpage,'td:nth-child(1)'))[14]
    distressed[i]          = html_text(html_nodes(webpage,'td:nth-child(2)'))[6]
    med.income[i]          = html_text(html_nodes(webpage,'td:nth-child(3)'))
    y.2016.ffiec.income[i] = html_text(html_nodes(webpage,'td:nth-child(4)'))
    y.2016.med.income[i]   = html_text(html_nodes(webpage,'td:nth-child(5)'))
    y.2010.med.income[i]   = html_text(html_nodes(webpage,'td:nth-child(6)'))
    population[i]          = html_text(html_nodes(webpage,'td:nth-child(7)'))
    minority.percentage[i] = html_text(html_nodes(webpage,'td:nth-child(8)'))
    minority.pop[i]        = html_text(html_nodes(webpage,'td:nth-child(9)'))
    owner.occupied[i]      = html_text(html_nodes(webpage,'td:nth-child(10)'))
    minor.family.unit[i]   = html_text(html_nodes(webpage,'td:nth-child(11)'))
  }
  else{
    income.level[i]        = ""
    distressed[i]          = ""
    med.income[i]          = ""
    y.2016.ffiec.income[i] = ""
    y.2016.med.income[i]   = ""
    y.2010.med.income[i]   = ""
    population[i]          = ""
    minority.percentage[i] = ""
    minority.pop[i]        = ""
    owner.occupied[i]      = ""
    minor.family.unit[i]   = ""
  }
  print(i)
}
time2 = Sys.time()


external.demo = data.table(
            "income.level"         = income.level,
            "distressed"           = distressed,
            "med.income"           = med.income,
            "y.2016.ffiec.income"  = y.2016.ffiec.income,
            "y.2016.med.income"    = y.2016.med.income,
            "y.2010.med.income"    = y.2010.med.income,
            "population"           = population,
            "minority.percentage"  = minority.percentage,
            "minority.pop"         = minority.pop,
            "owner.occupied"       = owner.occupied,
            "minor.family.unit"    = minor.family.unit)


fwrite(external.demo, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/external_demo_2.csv")