
#1 DHS country list
url<-("http://api.dhsprogram.com/rest/dhs/countries?f=json")

suppressPackageStartupMessages(library(jsonlite)) # for fromJSON
suppressPackageStartupMessages(library(data.table)) # for data.table
suppressPackageStartupMessages(library(dplyr))

# read DHS API country list 
jsondata<-fromJSON(url) 
# create data frame with countries 
ctry_DHS<-data.table(jsondata$Data)
# tidy up
ctry_DHS<-ctry_DHS %>%
  rename (country =   CountryName) %>% 
  rename (DHSregion1  =   RegionName) %>%
  rename (DHSregion2  =   SubregionName) %>%
  select (country, DHSregion1, DHSregion2)

#2 UNSD country list

url<-("https://unstats.un.org/unsd/methodology/m49/")

suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(dplyr))

# data table scraping from the web 
ctry_UNSD<-read_html(url) %>% 
  html_nodes("table") %>%
  .[[7]] %>% 
  html_table(header = TRUE)     
# tidy up
ctry_UNSD<-ctry_UNSD %>%
  rename (country =   "Country or Area") %>% 
  rename (M49 =   "M49 code") %>% 
  rename (ISOalpha3   =   "ISO-alpha3 code") %>% 
  select(country, M49, ISOalpha3) 

# Assign sub-region names
head(ctry_UNSD, 20)
str(ctry_UNSD)
ctry_UNSD$country<-as.character(ctry_UNSD$country)
ctry_UNSD<-ctry_UNSD %>% 
  mutate(
    UNSDsubregion=country,
    UNSDsubregion=ifelse(ISOalpha3!="", "", UNSDsubregion)
  )
for (i in 1:nrow(ctry_UNSD)){
  if (ctry_UNSD[i,4]==""){
    ctry_UNSD[i,4]=ctry_UNSD[i-1,4]
  }}
# Keep only country rows and replace country names as needed
ctry_UNSD<-ctry_UNSD %>% 
  filter(ISOalpha3!="") %>% 
  select(country, UNSDsubregion) %>% 
  mutate(
    country = ifelse(country == "Bolivia (Plurinational State of)", "Bolivia", country) ,
    country = ifelse(country == "Cabo Verde", "Cape Verde", country) , 
    country = ifelse(country == "Democratic Republic of the Congo", "Congo Democratic Republic", country) ,
    country = ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire", country) ,
    country = ifelse(country == "Kyrgyzstan", "Kyrgyz Republic", country) , 
    country = ifelse(country == "Republic of Moldova", "Moldova", country) , 
    country = ifelse(country == "United Republic of Tanzania", "Tanzania", country) ,
    country = ifelse(country == "Viet Nam", "Vietnam", country) 
  )

suppressPackageStartupMessages(library(Hmisc))
label(ctry_UNSD$UNSDsubregion) <- "Sub-region, UNSD Methodology 49"

#3 Merge DHS and UNSD M49 lists
dim(ctry_DHS)
dim(ctry_UNSD)
names(ctry_DHS)
names(ctry_UNSD)
obsDHS<-nrow(ctry_DHS)
obsUNSD<-nrow(ctry_UNSD)

ctry<-left_join(ctry_DHS, ctry_UNSD, by = "country")
#deal with Code d'Ivoire, Nigeria (Ondo State), and Turkey being na values

ctry<- mutate(ctry, 
              UNSDsubregion=ifelse(country=="Cote d'Ivoire", 
                                   "Western Africa", 
                                   UNSDsubregion) )

ctry<- mutate(ctry, 
              UNSDsubregion=ifelse(country=="Nigeria (Ondo State)", 
                                   "Western Africa", 
                                   UNSDsubregion) )

ctry<- mutate(ctry, 
              UNSDsubregion=ifelse(country=="Turkey", 
                                   "Western Asia", 
                                   UNSDsubregion) )

test<-filter(ctry, is.na(UNSDsubregion))
table(test$country)#there should be none after modifications


nrow(ctry)

table(ctry$UNSDsubregion, exclude = NULL)#there should be no <NA>'

#------------------------
#Code for higher-level classification
#------------------------
# can be created depending on study purposes. In this example, a three-category grouping is done.
ctry2<-ctry %>%
  mutate(
    studyregion="", 
    studyregion=ifelse(UNSDsubregion=="Middle Africa" | 
                         UNSDsubregion=="Western Africa", 
                       "Centera and Western Africa", studyregion), 
    studyregion=ifelse(UNSDsubregion=="Eastern Africa" | 
                         UNSDsubregion=="Southern Africa", 
                       "Southern and Eastern Africa", studyregion), 
    studyregion=ifelse(studyregion=="", 
                       "Other Regions", studyregion)
  )
addmargins(table(ctry2$UNSDsubregion, ctry2$studyregion))   

#------------------------
#Merge the two data frames: ctry & all_indicators (cleaned DHS API data).
#------------------------
# Inspect and confirm "country" variable
length(unique(all_indicators$Country)) #number of unique countries
length(unique(ctry$country)) #number of unique countries

# Merge UNSD country list with the DHS API data, "dta" 
dim(all_indicators)
dim(ctry)
group_region<-left_join(all_indicators, ctry, by = "country")
dim(group_region)
#length(unique(all_indicators$surveyid)) #number of unique surveys
length(unique(all_indicators$country)) #number of unique countries (36)

write.csv(group_region, paste0(here::here(), "/1-dm/countryindicatorsbyregion.csv"))
