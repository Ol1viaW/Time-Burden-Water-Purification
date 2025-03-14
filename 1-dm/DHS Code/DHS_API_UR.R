#need to run DHS_population.R
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
boil2 <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") 

boil2 <- boil2 %>%
  mutate(number_households_boil = Value * DenominatorWeighted)
total_households_boil_all_countries <- sum(boil2$number_households_boil) #total number of households that boil water in all countries
total_time_boil_all_countries <- total_households_boil_all_countries * 20 / 60 #each household spends 20 minutes boiling water and divide by 60 to convert minutes to hours

c = as.vector(countries$DHS_CountryCode) #for all countries
bleach3 <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", c, selectSurvey="byIndicator", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") 

boil <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="byIndicator", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil)[1] <- "Country" 
names(boil)[2] <- "Community Type" #label for Rural/Urban
names(boil)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
#able to pull data from all selected countries


boil_all <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil_all)[1] <- "Country" 
names(boil_all)[2] <- "Community Type" #label for Rural/Urban
names(boil_all)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
#able to pull data from all selected countries

boil_all_population <- merge(boil_all, population_urban_rural, by = c("Country", "Community Type"), all = TRUE)
names(boil_all_population)[5] <- "2006"
names(boil_all_population)[6] <- "2008"
names(boil_all_population)[7] <- "2012"
names(boil_all_population)[8] <- "2013"
names(boil_all_population)[9] <- "2014"
names(boil_all_population)[10] <- "2015"
names(boil_all_population)[11] <- "2016"
names(boil_all_population)[12] <- "2017"
names(boil_all_population)[13] <- "2018"
names(boil_all_population)[14] <- "2019"
names(boil_all_population)[15] <- "2020"
names(boil_all_population)[16] <- "2021"
names(boil_all_population)[17] <- "2022"

boil_all_population <- boil_all_population[order(boil_all_population$SurveyYear, decreasing = TRUE),]
boil <- boil[order(boil$SurveyYear, decreasing = TRUE),]

boil_all_population <- boil_all_population %>%
  mutate(population_2 = c("13853989", "19621881", "38358435", "15669052", "23994819", "6552761", "60117263", "55441746", "41471864", "24025884",
                          "15196430", "6904253", "12496843", "4092180", "13140099", "14338150", "17578693", "11336960",
                          "908684959", "487702168", "2009661","2488943",
                          "230087", "2012698", "NA", "NA", "10612900", "2222128", "4628214", "3418614",
                          "10073534", "7762359",
                          "1167112", "1706345", "119926424", "144572428", "945229", "9270152", "137526581", "78853074", "6517240", "2408285",
                          "1058009", "1807826", "9552623", "1350704", "4993082", "5720767", "27730", "176949", "36282093", "15610256", "19555396", "36866878", "859030", "365532",
                          "10281807", "17845914", "7788224", "7779195",
                          "54641528", "40950796",
                          "1217860", "986650",
                          "3620962", "1986238", "6900715", "22848874",
                          "3405346", "6475247", 
                          "4022610", "4461940",
                          "NA", "NA", "NA", "NA", "NA", "NA", "NA"))

names(boil_all_population)[2] = "Community"
boil_all_population <- boil_all_population %>%
  select(Country, Community, Boil, population_2) %>%
  drop_na(Boil)
boil_all_population <- boil_all_population[order(boil_all_population$Country, decreasing = FALSE),]
names(boil_all_population)[4] = "Community Population"
names(boil_all_population)[2] = "Community Type"



#-------- Times based on API pull of mean household with urban/rural split
pop <- merge(boil_all_population, household_size_u_r, 
             by = c("Country", "Community Type"), 
             all = TRUE)
pop <- pop %>%
  rename(
    Population = `Community Population`,    # Renaming Community Population to population
    Size = `Mean Household Size`  # Renaming Mean Household Size to Mean_Household_Size (you can change this to a different name if needed)
  )

# Convert "Population" and "Size" columns to numeric
pop$Population <- as.numeric(pop$Population)
pop$Size <- as.numeric(pop$Size)

test <- pop %>% 
  mutate(time = ifelse(is.na(Population) | is.na(Size), NA, (Boil / 100) * (Population / Size) * (30 / 60)))
#time is NA if population or size value for row is NA
test <- pop %>% mutate(time = (Boil/100)*(Population/Size)*(30/60)) 
test$time = as.numeric(test$time)
test = test %>% filter(!is.na(time))
total_time_selected_DHS_Countries = sum(test$time) #67177625

#-------- Yoshika added this
# 
# # population <- read_excel("~/Downloads/P_Data_Extract_From_Population_estimates_and_projections.xlsx")
# population <- read.csv(population) # Yoshika changed this -- can change back
# population <- population[ -c(2,4) ]
# names(population)[2] <- "CommunityType"
# names(population)[1] <- "Country"
# population$Country[population$Country=="Egypt, Arab Rep."] <- "Egypt"


# boil2y <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), c, surveyYearStart = 2000, selectSurvey="latest", breakdown = "national") %>%
#   rename(Country = CountryName)
# pop <- population %>% select(Country, X2022)
# test <- left_join(boil2y, pop, by=c("Country"))
# write.csv(test, paste0(data_dir, "test_boil.csv"))
# # assumptions: 4 people per household, daily tx, 30 min by boiling among self reported boilers
# test2 <- test %>% mutate(time = (Value/100)*(X2022/4)*(30/60))
# test2$time = as.numeric(test2$time)
# test2 = test2 %>% filter(!is.na(time))
# sum(test2$time)
#---------------

# testing API

#selecting based on wealth quintile rather than urban/rural
boil_wealth <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), c, surveyYearStart = 2000, selectSurvey="byIndicator", breakdown = "background") %>%
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

boil <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil)[1] <- "Country" 
names(boil)[2] <- "Community Type" #label for Rural/Urban
names(boil)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
boil <- boil[order(boil$SurveyYear, decreasing = TRUE),]
#able to pull data from all selected countries

# Households treating water by adding bleach/chlorine
bleach2 <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", c, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel) %>%
  filter(CountryName == "Nigeria")
names(bleach2)[1] <- "Country" 
names(bleach2)[2] <- "Community Type" #label for Rural/Urban
names(bleach2)[3] <- "Bleach" #rename value column, which is the percentage of population that uses bleach
#able to pull data from all selected countries



# Households treating water by adding bleach/chlorine
bleach <- dhs_data(indicatorIds = "WS_WTRT_H_BLC", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(bleach)[1] <- "Country" 
names(bleach)[2] <- "Community Type" #label for Rural/Urban
names(bleach)[3] <- "Bleach" #rename value column, which is the percentage of population that uses bleach
#able to pull data from all selected countries

# Households treating water by straining through a cloth
strain <- dhs_data(indicatorIds = "WS_WTRT_H_STN", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  #select columns of interest 
  select(CountryName, Value, CharacteristicLabel)  %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(strain)[1] <- "Country" 
names(strain)[3] <- "Strain" #rename value column
names(strain)[2] <- "Community Type" #label for Rural/Urban
#missing Angola and Jordon

# Households treating water by using a ceramic, sand or other filter
filter <- dhs_data(indicatorIds = "WS_WTRT_H_CER", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(filter)[1] <- "Country" 
names(filter)[2] <- "Community Type" #label for Rural/Urban
names(filter)[3] <- "Filter" #rename value column
#able to pull data from all selected countries

# Households treating water by using solar disinfection
sodis <- dhs_data(indicatorIds = "WS_WTRT_H_SOL", countryIds, surveyYearStart = 2000,selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(sodis)[1] <- "Country" 
names(sodis)[2] <- "Community Type" #label for Rural/Urban
names(sodis)[3] <- "SolarDisinfection" #rename value column
#missing Burundi, Jordon, Namibia

# collection time larger than 30 minutes
watercollect <- dhs_data(indicatorIds = "WS_Time_H_M30", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(watercollect)[1] <- "Country"
names(watercollect)[2] <- "Community Type" #label for Rural/Urban
names(watercollect)[3] <- "More than 30 minutes" #rename value column

# treats water using acceptable method
treatswater <- dhs_data(indicatorIds = "WS_WTRT_H_APP", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(treatswater)[1] <- "Country"
names(treatswater)[2] <- "Community Type" #label for Rural/Urban
names(treatswater)[3] <- "Treats water with acceptable method" #rename value column


# Percentage of households whose main source of drinking water is water piped into the dwelling 
# + Percentage of households whose main source of drinking water is water piped into the yard or plot
piped1 <- dhs_data(indicatorIds = "WS_SRCE_H_PYD", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(piped1)[1] <- "Country"
names(piped1)[2] <- "Community Type" #label for Rural/Urban
names(piped1)[3] <- "Piped1" #rename value column

piped2 <- dhs_data(indicatorIds = "WS_SRCE_H_PIP", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
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

#Error in handle_pagination_json(endpoint, query, all_results, timeout) : 
#Records returned equal to 0. Most likely your query terms are too specific or there is a typo that does not trigger a 404 or 500 error
# stand <- dhs_data(indicatorIds = "WS_WTRT_H_LET", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
#   filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
#   select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
#   relocate(CountryName,CharacteristicLabel)
# names(stand)[1] <- "Country"
# names(stand)[2] <- "Community Type" #label for Rural/Urban
# names(stand)[3] <- "LetStand" #rename value column

# female child under 15 collects water (% of population)
femaleunder15 <- dhs_data(indicatorIds = "WS_PCDW_P_CFM", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel)  %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(femaleunder15)[1] <- "Country" 
names(femaleunder15)[2] <- "Community Type" #label for Rural/Urban
names(femaleunder15)[3] <- "Female Under 15" #rename value column


# female over the age of 15 collects water (% of population)
femaleover15 <- dhs_data(indicatorIds = "WS_PCDW_P_AFM", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(femaleover15)[1] <- "Country" 
names(femaleover15)[2] <- "Community Type" #label for Rural/Urban
names(femaleover15)[3] <- "Female Over 15" #rename value column


household_size_u_r <- dhs_data(indicatorIds = c("HC_MEMB_H_MNM"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(household_size_u_r)[1] <- "Country" 
names(household_size_u_r)[2] <- "Community Type" #label for Rural/Urban
names(household_size_u_r)[3] <- "Mean Household Size" #rename value column
#selected DHS countries with urban and rural subdivision

anywater <- dhs_data(indicatorIds = "WS_WTRT_H_NTR", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
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
boila <- merge(boil, anywater, by = c("Country", "Community Type"), all = TRUE)
boil_bleach <- merge(boila, bleach, by = c("Country", "Community Type"), all = TRUE)
pipe <- merge(boil_bleach, piped, by = c("Country", "Community Type"), all = TRUE)
bleach_filter <- merge(filter, pipe, by = c("Country", "Community Type"), all = TRUE)
filter_sodis <- merge(sodis, bleach_filter, by = c("Country", "Community Type"), all = TRUE)
sodis_watercollect <- merge(filter_sodis, watercollect, by = c("Country", "Community Type"), all = TRUE)
watercollect_femaleunder15 <- merge(sodis_watercollect, femaleunder15, by = c("Country", "Community Type"), all = TRUE)
femaleunder15_strain <- merge(watercollect_femaleunder15, strain, by = c("Country", "Community Type"), all = TRUE)
femaleunder <- merge(femaleunder15_strain, household_size_u_r, by = c("Country", "Community Type"), all = TRUE)
femaleunderover <- merge(femaleunder, femaleover15, by = c("Country", "Community Type"), all = TRUE)
all_indicators_UR <- merge(femaleunderover, treatswater, by = c("Country", "Community Type"), all = TRUE) %>%
  relocate("Country", "Community Type","Treats water with acceptable method", "Any_water_treatment", "Boil", "Bleach", "Filter", "SolarDisinfection", "Strain", "More than 30 minutes")
names(all_indicators_UR)[1] <- "country" #rename value column

#write.csv(all_indicators, paste0(here::here(), "/1-dm/countryindicators.csv"))
# 
# # Households not treating water 
# #Error in handle_pagination_json(endpoint, query, all_results, timeout) : 
# #Records returned equal to 0. Most likely your query terms are too specific or there is a typo that does not trigger a 404 or 500 error
# notreatment <- dhs_data(indicatorIds = "WS_WTRT_H_NTR	", countryIds, surveyYearStart = 1900, selectSurvey="latest", breakdown = "background") %>%
#   filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
#   select(CountryName, Value, CharacteristicLabel) %>% #select columns of interest
#   relocate(CountryName, CharacteristicLabel)
# names(notreatment)[1] <- "Country" 
# names(notreatment)[2] <- "Community Type" #label for Rural/Urban
# names(notreatment)[3] <- "No treatment" #rename value column


#------------------------
# checking old surveys against Rosen and Clausen Paper
#------------------------

# Households treating water by boiling using outdated surveys published at or before 2007 to check numbers against Rosen and Clausen paper
boil_old <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), surveyYearEnd = 2007, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) #select columns of interest 
names(boil_old)[1] <- "Country" 
names(boil_old)[2] <- "Boil_old" #rename value column, which is the percentage of population that boils water
names(boil_old)[3] <- "Density Characteristic" #label for Rural/Urban


# Households treating water by bleach using outdated surveys to check numbers against Rosen and Clausen paper
bleach_old <- dhs_data(indicatorIds = c("WS_WTRT_H_BLC"), surveyYearEnd = 2007, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) #select columns of interest 
names(bleach_old)[1] <- "Country" 
names(bleach_old)[2] <- "Bleach_old" #rename value column, which is the percentage of population that boils water
names(bleach_old)[3] <- "Density Characteristic" #label for Rural/Urban

strain_old <- dhs_data(indicatorIds = c("WS_WTRT_H_STN"), surveyYearEnd = 2007, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) #select columns of interest 
names(strain_old)[1] <- "Country" 
names(strain_old)[2] <- "Strain_old" #rename value column, which is the percentage of population that boils water
names(strain_old)[3] <- "Density Characteristic" #label for Rural/Urban

#compare against Rosen and Clausen paper 
filter_old <- dhs_data(indicatorIds = c("WS_WTRT_H_CER"), surveyYearEnd = 2007, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) #select columns of interest 
names(filter_old)[1] <- "Country" 
names(filter_old)[2] <- "Filter_old" #rename value column, which is the percentage of population that boils water
names(filter_old)[3] <- "Density Characteristic" #label for Rural/Urban

sodis_old <- dhs_data(indicatorIds = c("WS_WTRT_H_SOL"), surveyYearEnd = 2007, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) #select columns of interest 
names(sodis_old)[1] <- "Country" 
names(sodis_old)[2] <- "SolarDisinfection_old" #rename value column, which is the percentage of population that boils water
names(sodis_old)[3] <- "Density Characteristic" #label for Rural/Urban

dhs_indicators(returnFields=c("IndicatorIdOld","Label","Definition")) #used to get indicatorIds

#------------------------
# collecting data from surveys without using API indicators
#------------------------
# 
# #rest of data for female children under 15 collecting water work in progress
# datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV", fileType = "PR")
# #data_download <- get_datasets(datasets$FileName) #can't download from DHS website
# data_download <- dhszips_out
# questions <- search_variable_labels(datasets$FileName, search_terms = "household")
# extract <- extract_dhs(questions, add_geo = FALSE)
# ques <- search_variables(data_download$FileName, variables = c("hv204", "hv003", "hv236"))
# extract2 <- extract_dhs(ques, add_geo = FALSE)
# extra2 <- head(extract2$TZPR7BSV) #just start with 1 country at a time
# 
# 
# 
# #rest of data for female children under 15 collecting water work in progress
# datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV", fileType = "PR")
# #data_download <- get_datasets(datasets$FileName) #can't download from DHS website
# data_download <- dhszips_out
# questions <- search_variable_labels(datasets$FileName, search_terms = "household")
# extract <- extract_dhs(questions, add_geo = FALSE)
# ques <- search_variables(datasets$FileName, variables = c("hv204", "hv003", "hv236"))
# extract2 <- extract_dhs(ques, add_geo = FALSE)
# extra2 <- head(extract2$TZPR7BSV) #just start with 1 country at a time
# 
# 
# # female over the age of 15 collects water (% of population)
# femaleover15 <- dhs_data(indicatorIds = "WS_PCDW_P_AFM", countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
#   filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
#   select(CountryName, Value, CharacteristicLabel) #select columns of interest
# names(femaleover15)[1] <- "Country" 
# names(femaleover15)[2] <- "Femaleover15" #rename value column
# names(femaleover15)[3] <- "Community Type" #label for Rural/Urban
# # 
# #------------- example
# datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV", fileType = "PR")
# #downloads <- client$get_datasets(datasets$FileName)
# 
# # and grab the questions from this again along with also questions detailing the province
# questions <- client$survey_variables(datasets$FileName, variables=c("hv237"))
# extract2 <- client$extract(questions, add_geo = FALSE)
# 
# # and now extract the data
# extract <- client$extract(questions, add_geo = FALSE)
# 
# # what does our extract look like
# str(extract)
# 
# ## test single dataset
# downloads <- paste0(box_path, "data/dhs/rdhs_download/")
# test = readRDS(downloads$LBPR7ASV)
# 
# #------------table
# # hhwt table - work in progress
# idvars<-c("CountryName", "SurveyId")
# hhwttable <- boil %>%
#   full_join(bleach, by = idvars) %>%
#   full_join(strain, by = idvars) %>%
#   full_join(filter, by = idvars) %>%
#   full_join(sodis, by = idvars)
# 
# write.csv(boil, paste0(here::here(), "/1-dm/surveylist.csv"))
# 
# # define merge ID variables
# idvars<-c("Country", "Community Type")
# 
# dtaapi<-boil %>%
#   full_join(bleach, by =idvars) %>%
#   full_join(filter, by =idvars) %>%
#   full_join(femaleunder15, by =idvars) 
# 
# dim(dtaapi)
# names(dtaapi)
# table(dtaapi$CharacteristicCategory)
# 
# obs<-nrow(dtaapi)
# surveys<-length(unique(dtaapi$SurveyId))