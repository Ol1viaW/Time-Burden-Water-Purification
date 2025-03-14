# download DHS data using rdhs package

# this script is a work in progress

#devtools::install_github("ropensci/rdhs")
library(rdhs)

## set up your credentials
config <- set_rdhs_config(email = "ycrider@berkeley.edu",
                project = "Safe water and women's time",
                config_path = "rdhs.json",
                cache_path = paste0(box_path, "data/dhs/rdhs_download"),
                global = FALSE)
client <- client_dhs(config)
client

countries = dhs_countries(                                  
  returnFields = c("CountryName", "DHS_CountryCode")
) %>% select(DHS_CountryCode) # add filter step here for just the countries we're using DHS data for

c = as.vector(countries$DHS_CountryCode)
survs <- dhs_surveys(countryIds = c, # replace with c later, test with AF
                     surveyType = "DHS",
                     selectSurveys = "latest",
                     surveyYearStart = 2013)

datasets = dhs_datasets(surveyIds = survs$SurveyId,
                        fileFormat = "SV",
                        fileType = "PR")

d = str(datasets)
write.csv(datasets, paste0(data_dir, "survey_list.csv"))
# load data
downloads = get_datasets(datasets$FileName)

# testing API
# Households treating water by boiling
boil <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select() %>% #select columns of interest
  rename(boil = Value)

# Households treating water by adding bleach/chlorine
bleach <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", surveyYearStart = 2000,selectSurvey="latest", breakdown = "national")

# Households treating water by straining through a cloth
strain <- dhs_data(indicatorIds = "WS_WTRT_H_STN", surveyYearStart = 2000,selectSurvey="latest", breakdown = "national")

# Households treating water by using a ceramic, sand or other filter
filter <- dhs_data(indicatorIds = "WS_WTRT_H_CER", surveyYearStart = 2000,selectSurvey="latest", breakdown = "national")

# Households treating water by using solar disinfection
sodis <- dhs_data(indicatorIds = "WS_WTRT_H_SOL", surveyYearStart = 2000,selectSurvey="latest", breakdown = "national")

# female child under 15 collects water (% of population)
femaleunder15 <- dhs_data(indicatorIds = "WS_PCDW_P_CFM", surveyYearStart = 2000,selectSurvey="latest", breakdown = "national")

#
femaleover15 <- dhs_data(indicatorIds = "WS_PCDW_P_AFM", surveyYearStart = 2000,selectSurvey="latest", breakdown = "national")


# hhwt table - work in progress
idvars<-c("CountryName", "SurveyId")
hhwttable <- boil %>%
  full_join(bleach, by = idvars) %>%
  full_join(strain, by = idvars) %>%
  full_join(filter, by = idvars) %>%
  full_join(sodis, by = idvars)

write.csv(boil, paste0(here(), "/1-dm/surveylist.csv"))

#------------- example
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV", fileType = "PR")
#downloads <- client$get_datasets(datasets$FileName)

# and grab the questions from this again along with also questions detailing the province
questions <- client$survey_variables(datasets$FileName, variables=c("hv237"))
extract2 <- client$extract(questions, add_geo = FALSE)

# and now extract the data
extract <- client$extract(questions, add_geo = FALSE)

# what does our extract look like
str(extract)

## test single dataset
downloads <- paste0(box_path, "data/dhs/rdhs_download/")
test = readRDS(downloads$LBPR7ASV)