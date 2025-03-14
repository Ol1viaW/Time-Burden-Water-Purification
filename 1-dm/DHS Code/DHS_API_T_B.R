# download DHS data using rdhs package

# this script is a work in progress

#devtools::install_github("ropensci/rdhs")
#library(rdhs)

## set up your credentials
config <- set_rdhs_config(email = "oliviawahl@berkeley.edu",
                          project = "Investigating household water fetching",
                          config_path = "rdhs.json",
                          cache_path = paste0(box_path, "data/dhs/rdhs_download"),
                          global = FALSE)
client <- client_dhs(config)
client

countries = dhs_countries(                                  
  returnFields = c("CountryName", "DHS_CountryCode")
) %>% select(DHS_CountryCode) 
# add filter step here for just the countries we're using DHS data for

c = as.vector(countries$DHS_CountryCode) #for all countries
print(c)
survs <- dhs_surveys(countryIds = c("AL","AO","AM","AZ","BO",
                                    "BF","BU", "KH", "CI", "EG", 
                                    "GA", "GM", "GH", "GU", "HT", 
                                    "IA", "ID", "JO", "KE", "KY", 
                                    "MD", "MV", "MR","MM", "NM", "NG",
                                    "NP", "PK", "PNG", "PE", "PH", 
                                    "RW", "SL", "ZA", "TJ", "TZ", 
                                    "TL","ZM"),
                     # replace with c later, test with AF
                     surveyType = "DHS",
                     selectSurveys = "latest",
                     surveyYearStart = 2013)

datasets = dhs_datasets(surveyIds = survs$SurveyId,
                        fileFormat = "SV",
                        fileType = "PR")

d = str(datasets)
#write.csv(datasets, paste0(data_dir, "survey_list.csv"))
# load data
#downloads = get_datasets(datasets$FileName)
#can't run because can't connect and log into DHS website likely due to lack of download priveleges but doesn't seem to be an issue to run rest of the code
countryIds = c("AL","AO","AM","AZ","BO",
               "BF","BU", "KH", "CI", "EG", 
               "GA", "GM", "GH", "GU", "HT", 
               "IA", "ID", "JO", "KE", "KY", 
               "MD", "MV", "MR","MM", "NM", "NG", 
               "NP", "PK", "PNG", "PE", "PH", 
               "RW", "SL", "ZA", "TJ", "TZ", 
               "TL","ZM")

#------------------------
# Finding time spent boiling water globaly
#------------------------
#the data for the number of households(weighted number of households based on DHS API weighting) in all DHS countries that boil water 
#multiplied that by 20 and divided by 60 to get globally 8,487,840.43 hrs spent on boiling water also this is all DHS countries (50 countries) 
countrys = dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))
boil2 <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") 

boil2 <- boil2 %>%
  mutate(number_households_boil = Value * DenominatorWeighted)
total_households_boil_all_countries <- sum(boil2$number_households_boil) #total number of households that boil water in all countries
total_time_boil_all_countries <- total_households_boil_all_countries * 20 / 60 #each household spends 20 minutes boiling water and divide by 60 to convert minutes to hours

c = as.vector(countries$DHS_CountryCode) #for all countries
bleach3 <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", c, selectSurvey="byIndicator", breakdown = "national") 

household_size_u_r <- dhs_data(indicatorIds = c("HC_MEMB_H_MNM"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(household_size_u_r)[1] <- "Country" 
names(household_size_u_r)[2] <- "Community Type" #label for Rural/Urban
names(household_size_u_r)[3] <- "Mean Household Size" #rename value column
#selected DHS countries with urban and rural subdivision

household_size_total <- dhs_data(indicatorIds = c("HC_MEMB_H_MNM"), c, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, SurveyYear) %>% #select columns of interest
  relocate(CountryName,Value)
names(household_size_total)[1] <- "Country" 
names(household_size_total)[2] <- "Mean Household Size" #label for Rural/Urban
#all DHS countries

# testing API

#selecting based on wealth quintile rather than urban/rural
boil_wealth <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), c, surveyYearStart = 2000, selectSurvey="byIndicator", breakdown = "national") %>%
  filter(CharacteristicCategory == "Wealth quintile") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil_wealth)[1] <- "Country" 
names(boil_wealth)[2] <- "Community Type" #label for Rural/Urban
names(boil_wealth)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
#able to pull data from all selected countries


# Households treating water by boiling categorized by urban/rural and DHS wealth index (lowest, second, middle, fourth, highest)
boil_wealth <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "all") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural" | CharacteristicLabel == "Lowest" | CharacteristicLabel == "Second"| CharacteristicLabel == "Fourth"| CharacteristicLabel == "Highest") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil_wealth)[1] <- "Country" 
names(boil_wealth)[2] <- "Community Type" #label for Rural/Urban
names(boil_wealth)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
#able to pull data from all selected countries

boil <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil)[1] <- "Country" 
names(boil)[2] <- "Community Type" #label for Rural/Urban
names(boil)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
boil <- boil[order(boil$SurveyYear, decreasing = TRUE),]
#able to pull data from all selected countries

# Households treating water by adding bleach/chlorine
bleach2 <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", c, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel) %>%
  filter(CountryName == "Nigeria")
names(bleach2)[1] <- "Country" 
names(bleach2)[2] <- "Community Type" #label for Rural/Urban
names(bleach2)[3] <- "Bleach" #rename value column, which is the percentage of population that uses bleach
#able to pull data from all selected countries



# Households treating water by adding bleach/chlorine
bleach <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(bleach)[1] <- "Country" 
names(bleach)[2] <- "Community Type" #label for Rural/Urban
names(bleach)[3] <- "Bleach" #rename value column, which is the percentage of population that uses bleach
#able to pull data from all selected countries

# Households treating water by straining through a cloth
strain <- dhs_data(indicatorIds = "WS_WTRT_H_STN", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "national") %>%
  #select columns of interest 
  select(CountryName, Value, CharacteristicLabel)  %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(strain)[1] <- "Country" 
names(strain)[3] <- "Strain" #rename value column
names(strain)[2] <- "Community Type" #label for Rural/Urban
#missing Angola and Jordon

# Households treating water by using a ceramic, sand or other filter
filter <- dhs_data(indicatorIds = "WS_WTRT_H_CER", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(filter)[1] <- "Country" 
names(filter)[2] <- "Community Type" #label for Rural/Urban
names(filter)[3] <- "Filter" #rename value column
#able to pull data from all selected countries

# Households treating water by using solar disinfection
sodis <- dhs_data(indicatorIds = "WS_WTRT_H_SOL", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(sodis)[1] <- "Country" 
names(sodis)[2] <- "Community Type" #label for Rural/Urban
names(sodis)[3] <- "SolarDisinfection" #rename value column
#missing Burundi, Jordon, Namibia

# collection time larger than 30 minutes
watercollect <- dhs_data(indicatorIds = "WS_Time_H_M30", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(watercollect)[1] <- "Country"
names(watercollect)[2] <- "Community Type" #label for Rural/Urban
names(watercollect)[3] <- "More than 30 minutes" #rename value column

# treats water using acceptable method
treatswater <- dhs_data(indicatorIds = "WS_WTRT_H_APP", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(treatswater)[1] <- "Country"
names(treatswater)[2] <- "Community Type" #label for Rural/Urban
names(treatswater)[3] <- "Treats water with acceptable method" #rename value column


# Percentage of households whose main source of drinking water is water piped into the dwelling 
# + Percentage of households whose main source of drinking water is water piped into the yard or plot
piped1 <- dhs_data(indicatorIds = "WS_SRCE_H_PYD", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(piped1)[1] <- "Country"
names(piped1)[2] <- "Community Type" #label for Rural/Urban
names(piped1)[3] <- "Piped1" #rename value column

piped2 <- dhs_data(indicatorIds = "WS_SRCE_H_PIP", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(piped2)[1] <- "Country"
names(piped2)[2] <- "Community Type" #label for Rural/Urban
names(piped2)[3] <- "Piped2" #rename value column

piped <- merge(piped1, piped2, by = c("Country", "Community Type"))
piped <- piped %>%
  mutate(`Piped onto premises` = Piped1 + Piped2)
piped <- piped %>%
  select(-Piped1, -Piped2)

# female child under 15 collects water (% of population)
femaleunder15 <- dhs_data(indicatorIds = "WS_PCDW_P_CFM", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel)  %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(femaleunder15)[1] <- "Country" 
names(femaleunder15)[2] <- "Community Type" #label for Rural/Urban
names(femaleunder15)[3] <- "Female Under 15" #rename value column


# female over the age of 15 collects water (% of population)
femaleover15 <- dhs_data(indicatorIds = "WS_PCDW_P_AFM", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(femaleover15)[1] <- "Country" 
names(femaleover15)[2] <- "Community Type" #label for Rural/Urban
names(femaleover15)[3] <- "Female Over 15" #rename value column

anywater <- dhs_data(indicatorIds = "WS_WTRT_H_NTR", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(anywater)[1] <- "Country" 
names(anywater)[2] <- "Community Type" #label for Rural/Urban
names(anywater)[3] <- "Any_water_treatment" #rename value column
anywater$Any_water_treatment <- 100 - anywater$Any_water_treatment


#switch from boil_all_population to just boil_population of selected DHS countries but right now should be dropping unwanted countries
# merge two data frames by ID
# Merge data frames on multiple columns
#merged_df = pd.merge(boil, bleach, on=('Country', 'Community Type'), how='inner')
boil2 <- merge(boil, anywater, by = c("Country", "Community Type"), all = TRUE)
boil_bleach <- merge(boil2, bleach, by = c("Country", "Community Type"), all = TRUE)
pipe <- merge(boil_bleach, piped, by = c("Country", "Community Type"), all = TRUE)
bleach_filter <- merge(filter, pipe, by = c("Country", "Community Type"), all = TRUE)
filter_sodis <- merge(sodis, bleach_filter, by = c("Country", "Community Type"), all = TRUE)
sodis_watercollect <- merge(filter_sodis, watercollect, by = c("Country", "Community Type"), all = TRUE)
watercollect_femaleunder15 <- merge(sodis_watercollect, femaleunder15, by = c("Country", "Community Type"), all = TRUE)
femaleunder15_strain <- merge(watercollect_femaleunder15, strain, by = c("Country", "Community Type"), all = TRUE)
femaleunder <- merge(femaleunder15_strain, femaleover15, by = c("Country", "Community Type"), all = TRUE)
femaleunderover <- merge(femaleunder, household_size_u_r, by = c("Country", "Community Type"), all = TRUE)
all_indicators_T <- merge(femaleunderover, treatswater, by = c("Country", "Community Type"), all = TRUE) %>%
  relocate("Country", "Community Type","Treats water with acceptable method", "Any_water_treatment", "Boil", "Bleach", "Filter", "SolarDisinfection", "Strain", "More than 30 minutes")
names(all_indicators_T)[1] <- "country" #rename value column
#all_indicators_T <- all_indicators_T %>%
#  mutate(`Community Population` = 0)
#write.csv(all_indicators, paste0(here::here(), "/1-dm/countryindicators.csv"))
# 
# # Households not treating water 
# #Error in handle_pagination_json(endpoint, query, all_results, timeout) : 
# #Records returned equal to 0. Most likely your query terms are too specific or there is a typo that does not trigger a 404 or 500 error
# notreatment <- dhs_data(indicatorIds = "WS_WTRT_H_NTR	", countryIds, surveyYearStart = 1900, selectSurvey="latest", breakdown = "national") %>%
#   filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
#   select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
#   relocate(CountryName, CharacteristicLabel)
# names(notreatment)[1] <- "Country" 
# names(notreatment)[2] <- "Community Type" #label for Rural/Urban
# names(notreatment)[3] <- "No treatment" #rename value column
