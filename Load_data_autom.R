
rm(list=ls()) #clean up the memory

## Load packages
library(tidyverse)
library(fs)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(rsconnect)

## read some Data
DownloadTheCOVIDData<- function(){
  download.file(
    url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile= "F:/Proj_R/Covid-19_Eric_dashboard/data/covid19JH.zip"
  )
  data_path<- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  unzip(
    zipfile = "F:/Proj_R/Covid-19_Eric_dashboard/data/covid19JH.zip",
    files=paste0(data_path, c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv")),
    exdir = "F:/Proj_R/Covid-19_Eric_dashboard/data",
    junkpaths = T 
  )
}

UpdateMyData <- function(){
  T_refresh=0.01 #hours
  if(!dir.exists("F:/Proj_R/Covid-19_Eric_dashboard/data")){
    dir.create("data") 
    DownloadTheCOVIDData()
  }
  else if((!file.exists("F:/Proj_R/Covid-19_Eric_dashboard/data/covid19JH.zip")) || (as.double(Sys.time() - file_info("F:/Proj_R/Covid-19_Eric_dashboard/data/covid19JH.zip")$change_time, units="hours") > T_refresh)){
    DownloadTheCOVIDData()
  }
}

UpdateMyData()

total_cases <- read_csv("F:/Proj_R/Covid-19_Eric_dashboard/data/time_series_covid19_confirmed_global.csv", col_types = cols())
total_deaths <- read_csv("F:/Proj_R/Covid-19_Eric_dashboard/data/time_series_covid19_deaths_global.csv", col_types = cols())
total_recovered <- read_csv("F:/Proj_R/Covid-19_Eric_dashboard/data/time_series_covid19_recovered_global.csv", col_types = cols())


row_data2<-read.csv("F:/Proj_R/Covid-19_Eric_dashboard/data/single_nations.csv", header = T)
row_data2$iso_code<-as.character(row_data2$iso_code)
row_data2$country<-as.character(row_data2$country)

# ##Function inspired:  ------------------------------------------------------------------------------------------
# ##https://joachim-gassen.github.io/2020/03/tidying-the-new-johns-hopkins-covid-19-datasests/
extend_data <- function(x) {
  ## df_str <- deparse(substitute(df))
  ## var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  var_str <- deparse(substitute(x))
  
  x[which(x$`Province/State`=="Faroe Islands"), 2]=c("Faroe Islands")
  x[which(x$`Province/State`=="Greenland"), 2]=c("Greenland")
  x[which(x$`Province/State`=="French Guiana"), 2]=c("French Guiana")
  x[which(x$`Province/State`=="French Polynesia"), 2]=c("French Polynesia")
  x[which(x$`Province/State`=="Guadeloupe"), 2]=c("Guadeloupe")
  x[which(x$`Province/State`=="Mayotte"), 2]=c("Mayotte")
  x[which(x$`Province/State`=="New Caledonia"), 2]=c("New Caledonia")
  x[which(x$`Province/State`=="Reunion"), 2]=c("Reunion")
  x[which(x$`Province/State`=="Saint Barthelemy"), 2]=c("Saint Barthelemy")
  x[which(x$`Province/State`=="St Martin"), 2]=c("St Martin")
  x[which(x$`Province/State`=="Martinique"), 2]=c("Martinique")
  x[which(x$`Province/State`=="Aruba"), 2]=c("Aruba")
  x[which(x$`Province/State`=="Curacao"), 2]=c("Curacao")
  x[which(x$`Province/State`=="Sint Maarten"), 2]=c("Sint Maarten")
  x[which(x$`Province/State`=="Bermuda"), 2]=c("Bermuda")
  x[which(x$`Province/State`=="Cayman Islands"), 2]=c("Cayman Islands")
  x[which(x$`Province/State`=="Channel Islands"), 2]=c("Channel Islands")
  x[which(x$`Province/State`=="Gibraltar"), 2]=c("Gibraltar")
  x[which(x$`Province/State`=="Isle of Man"), 2]=c("Isle of Man")
  x[which(x$`Province/State`=="Montserrat"), 2]=c("Montserrat")
  x[which(x$`Province/State`=="Anguilla"), 2]=c("Anguilla")
  x[which(x$`Province/State`=="British Virgin Islands"), 2]=c("British Virgin Islands")
  x[which(x$`Province/State`=="Turks and Caicos Islands"), 2]=c("Turks and Caicos Islands")
  x[which(x$`Province/State`=="Bonaire, Sint Eustatius and Saba"), 2]=c("Bonaire, Sint Eustatius and Saba")
  x[which(x$`Province/State`=="Falkland Islands (Malvinas)"), 2]=c("Falkland Islands (Malvinas)")
  x[which(x$`Province/State`=="Saint Pierre and Miquelon"), 2]=c("Saint Pierre and Miquelon")
  
  x[which(x$`Country/Region`=="MS Zaandam"), 2]=c("Netherlands")
  x[which(x$`Country/Region`=="Holy See"), 2]=c("Vatican")
  x[which(x$`Country/Region`=="Congo (Brazzaville)"), 2]=c("Congo, Rep.")
  x[which(x$`Country/Region`=="Congo (Kinshasa)"), 2]=c("Congo, Dem. Rep.")
  x[which(x$`Country/Region`=="Diamond Princess"), 2]=c("United Kingdom")
  
  x<-x %>%
    rename(country = `Country/Region`)
  x<-left_join(x, row_data2[,c(1, 2)],by=c("country"))
  
  x %>%
    select(-`Province/State`, -Lat, -Long) %>%
    select(-country) %>%
    group_by(iso_code) %>%
    summarise_at(vars(-group_cols()), sum) %>%
    pivot_longer(
      -iso_code,
      names_to = "date_data",
      values_to = var_str) %>%
    ungroup() %>%
    mutate(date = mdy(date_data)) %>%
    select(iso_code, date, !! sym(var_str))
}
##----------------------------------------------
##------Run the function------------------------
row_data1 <- extend_data(total_cases) %>%
  full_join(extend_data(total_deaths), by = c("iso_code", "date")) %>%
  full_join(extend_data(total_recovered), by = c("iso_code", "date"))
##----------------------------------------------

##------------------Including new_cases, new_deaths and new_recovered, ----------------
row_data1<-mutate(row_data1, new_cases=0, new_deaths=0, new_recovered=0)

row_data1 <- row_data1 %>% 
  filter(!is.na(iso_code))

count_codes<-row_data1 %>%
  select(iso_code) %>%
  distinct(iso_code)

for (i in 1:nrow(count_codes)){
  Vect_1<-which(row_data1$iso_code==c(count_codes[i,1]))
  xmin=min(Vect_1)
  xmax=max(Vect_1)
  N=xmax-xmin
  row_data1[xmin, "new_cases"]=row_data1[xmin, "total_cases"]
  row_data1[xmin, "new_deaths"]=row_data1[xmin, "total_deaths"]
  row_data1[xmin, "new_recovered"]=row_data1[xmin, "total_recovered"]
  for (j in 1:N){
    row_data1[xmin+j, "new_cases"]=row_data1[xmin+j, "total_cases"]-row_data1[xmin+j-1, "total_cases"]
    row_data1[xmin+j, "new_deaths"]=row_data1[xmin+j, "total_deaths"]-row_data1[xmin+j-1, "total_deaths"]
    row_data1[xmin+j, "new_recovered"]=row_data1[xmin+j, "total_recovered"]-row_data1[xmin+j-1, "total_recovered"]
  }
}
##---------------------------------------------------------------------------


##Write csv files in the disk
write.csv(x=row_data1, file = "F:/Proj_R/Covid-19_Eric_dashboard/data/row_data1.csv")
write.csv(x=row_data2, file = "F:/Proj_R/Covid-19_Eric_dashboard/data/row_data2.csv")

# World_count <- st_read(dsn="data/World_modified_4.shp", quiet= TRUE) # shapefile loaded with sf package 
# save(World_count, file="data/Countries.RData")
# load("data/Countries.RData")

rsconnect::setAccountInfo(name='ecabrerae', token='771B63A519542402ED1C389071EB0951', secret='HtZuzF4OyYoG810h4w7u10QhICIVE9W2f6g1sVek')
rsconnect::deployApp('F:/Proj_R/Covid-19_Eric_dashboard/covid19_world_eric_db.Rmd', launch.browser = FALSE)
Y
