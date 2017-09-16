# install.packages("rvest")
library(rvest)
library(data.table)
library(stringr) #regrex


#---------------------------------------------------------#

#                      Data loading                       #

#---------------------------------------------------------#

path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

properties = fread(paste0(path, "properties_v2.csv"))
census = unique(properties$rawcensustractandblock)
census = as.character(census)

tract.number = str_sub(census,5,11)
county = str_sub(census,2,4)

demo.data = data.table("tract.number" = tract.number, "county" = county)
demo.data = demo.data[county < 999, ]
demo.data = demo.data[!duplicated(demo.data), ]
rm(properties)
gc()

income.level 				        = numeric(nrow(demo.data))
y.2010.msa.income         	= numeric(nrow(demo.data))
y.2016.ffiec.income         = numeric(nrow(demo.data))
below.poverty.line          = numeric(nrow(demo.data))
tract.family.income.per     = numeric(nrow(demo.data))
tract.median.income.do      = numeric(nrow(demo.data))
y.2016.median.income        = numeric(nrow(demo.data))
y.2010.house.income         = numeric(nrow(demo.data))

time1 = Sys.time()
for (i in 1:nrow(demo.data)){
  Sys.sleep(10)
  i = 1
  county = demo.data$county[i]
  tract = demo.data$tract.number[i]
  url1 = "https://www.ffiec.gov/census/report.aspx?year=2016&county="
  url2 = "&tract="
  url3 = "&state=06&report=income"
  url = paste0(url1, county, url2, tract, url3)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  webpage <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  if (!is.na(html_text(html_nodes(webpage,'td:nth-child(1)'))[14])){
    income.level[i]             = html_text(html_nodes(webpage,'td:nth-child(1)'))[14]
    y.2010.msa.income[i]        = html_text(html_nodes(webpage,'td:nth-child(2)'))[6]
    y.2016.ffiec.income[i]      = html_text(html_nodes(webpage,'td:nth-child(3)'))
    below.poverty.line[i]       = html_text(html_nodes(webpage,'td:nth-child(4)'))
    tract.family.income.per[i]  = html_text(html_nodes(webpage,'td:nth-child(5)'))
    tract.median.income.do[i]   = html_text(html_nodes(webpage,'td:nth-child(6)'))
    y.2016.median.income[i]     = html_text(html_nodes(webpage,'td:nth-child(7)'))
    y.2010.house.income[i]      = html_text(html_nodes(webpage,'td:nth-child(8)'))
  }
  else{
    income.level[i]            = ""
    y.2010.msa.income[i]       = ""
    y.2016.ffiec.income[i]     = ""
    below.poverty.line[i]      = ""
    tract.family.income.per[i] = ""
    tract.median.income.do[i]  = ""
    y.2016.median.income[i]    = ""
    y.2010.house.income[i]     = ""
  }
  print(i)
}
time2 = Sys.time()
time2 - time1

external.income = data.table(
  "income.level"              = income.level,
  "y.2010.msa.income"         = y.2010.msa.income,
  "y.2016.ffiec.income"       = y.2016.ffiec.income,
  "below.poverty.line"        = below.poverty.line,
  "tract.family.income.per"   = tract.family.income.per,
  "tract.median.income.do"    = tract.median.income.do,
  "y.2016.median.income"      = y.2016.median.income,
  "y.2010.house.income"       = y.2010.house.income)


fwrite(external.income, "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/external_income.csv")