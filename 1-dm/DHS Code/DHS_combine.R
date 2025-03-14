#need to run DHS_API_UR.R and DHS_API_T.R
DHS <- rbind(all_indicators_UR, all_indicators_T)
DHS <- DHS %>%
  arrange(country)

DHS <- DHS %>%
  mutate(`SurveyYear` = as.numeric(`SurveyYear`))

# DHS <- DHS %>%
#   group_by(country) %>%
#   mutate(
#     Total_Population = sum(`Community Population`, na.rm = TRUE)  # Total for Urban and Rural
#   ) %>%]]
#   ungroup() %>%
#   mutate(
#     `Community Population` = ifelse(`Community Type` == "Total",
#                                     Total_Population,  # Assign total to Total
#                                     `Community Population`)  # Keep Urban values as is
#   ) %>%
#   select(-Total_Population)


DHS <- DHS %>%
  group_by(country) %>%
  mutate(
    `Female Any Age` = `Female Under 15` + `Female Over 15`
  ) %>%
  ungroup()

DHS <- DHS %>%
  mutate(Type = 'DHS')

DHS_MICS <- bind_rows(DHS, MICS)

DHS_MICS <- DHS_MICS %>%
  arrange(country)
write_xlsx(MICS, path = "DHS_MICS_March_3.csv") 

DHS <- read_csv("Desktop/GitHub/safewatertime/Data:Results/DHS_MICS_March_3.csv")

#adding in population

population <- read_excel("~/Library/CloudStorage/Box-Box/safewatertime/data/P_Data_Extract_From_Population_estimates_and_projections-2.xlsx")
population <- population[ -c(2,4) ]
names(population)[2] <- "CommunityType"
names(population)[1] <- "Country"
population$Country[population$Country=="Egypt, Arab Rep."] <- "Egypt"

# Split name column into firstname and last name
# population <- population %>% separate(CommunityType, c('CommunityType', 'drop'))
# population$CommunityType<-gsub("population","",as.character(population$CommunityType))
# population <- population[-(3)]
# population_urban_rural <- population
# population_total <- population
# population_urban_rural$CommunityType <- gsub("Population","",as.character(population_urban_rural$CommunityType))
# population_urban_rural <- population_urban_rural[!(population_urban_rural$CommunityType == ""), ]
# 
# population_total$CommunityType <- gsub("Rural","",as.character(population_total$CommunityType))
# population_total <- population_total[!(population_total$CommunityType == ""), ]
# population_total$CommunityType <- gsub("Urban","",as.character(population_total$CommunityType))
# population_total <- population_total[!(population_total$CommunityType == ""), ]
# colnames(population_total) <- gsub(" \\[YR.*\\]", "", colnames(population_total))
# colnames(population_urban_rural) <- gsub(" \\[YR.*\\]", "", colnames(population_urban_rural))

# Clean and process the data in a single dataframe
population <- population %>%
  separate(CommunityType, c('CommunityType', 'drop'), remove = TRUE) %>% # Separate and drop the second column
  mutate(CommunityType = gsub("population", "", CommunityType)) %>% # Remove "population" from CommunityType
  select(-drop) # Remove the extra column created by `separate`

# Clean column names to remove unnecessary text
colnames(population) <- gsub(" \\[YR.*\\]", "", colnames(population))

# Rename the second column to "Community Type"
names(population)[2] <- "Community Type"

population$`2023` <- population$`2022` #duplicating 2022 column
population$`2005` <- population$`2006` #duplicating 2006 column

#relabel the Community Type values of "Population" to "Total" 
population$`Community Type` <- gsub("Population", "Total", population$`Community Type`)

library(readr)
#DHS_MICS <- read_csv("Desktop/GitHub/safewatertime/Data:Results/DHS_MICS_March_3.csv")

population_long <- population %>%
  pivot_longer(
    cols = starts_with("20"), # Select columns that start with years (e.g., 2006, 2008)
    names_to = "SurveyYear",  # Create a column called SurveyYear
    values_to = "Population" # Create a column for population values
  ) %>%
  mutate(SurveyYear = as.integer(SurveyYear)) # Ensure SurveyYear is numeric

# Join MICS_DHS with population_long
DHS_MICS_pop <- DHS_MICS %>%
  left_join(
    population_long,
    by = c("country" = "Country", "SurveyYear", "Community Type")
  )

library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

hh_sizes <- read_excel("Library/CloudStorage/Box-Box/safewatertime/data/Fixed_HH_Sizes.xlsx")
hh_sizes$year <- str_sub(hh_sizes$Date, -2, -1)
DHS_MICS_pop$year <- str_sub(DHS_MICS_pop$SurveyYear, -2, -1)
hh_sizes$year <- as.numeric(hh_sizes$year)

DHS_MICS_pop$year <- as.numeric(DHS_MICS_pop$year)

data <- DHS_MICS_pop

hh_sizes <- hh_sizes %>%
  rename(country = Country)

find_closest_year <- function(year, target_country, dataset) {
  print(paste("Processing country:", target_country, "for year:", year))
  # Filter by the specific country
  country_data <- dataset %>% filter(country == target_country)
  
  # Check if there are any rows for the country
  if (nrow(country_data) == 0) {
    return(NA) # No data for this country
  }
  
  # Find the closest year
  closest_year <- country_data$year[which.min(abs(country_data$year - year))]
  return(closest_year)
}

data <- data %>%
  mutate(country = as.character(country), year = as.numeric(year))

hh_sizes <- hh_sizes %>%
  mutate(
    country = as.character(country),
    year = as.numeric(year),
    Size = ifelse(is.na(Size), NA, as.numeric(Size)) # Keep NA as NA
  )

hh_sizes <- hh_sizes %>%
  group_by(country, year) %>%
  summarize(Size = mean(Size, na.rm = TRUE), .groups = "drop")

# Apply the find_closest_year function
data <- data %>%
  mutate(
    closest_year_hh_sizes = mapply(
      find_closest_year, 
      year = year, 
      target_country = country, 
      MoreArgs = list(dataset = hh_sizes)
    )
  )

data <- data %>%
  mutate(closest_year_hh_sizes = as.numeric(closest_year_hh_sizes))

hh_sizes <- hh_sizes %>%
  mutate(year = as.numeric(year))

# Perform the left join using closest year
merged_data <- data %>%
  left_join(hh_sizes, by = c("closest_year_hh_sizes" = "year", "country" = "country"))

merged_data <- merged_data %>%
  select(-year, -closest_year_hh_sizes) %>%
  rename(avg_HH_size = Size)


pop <- read_excel("~/Library/CloudStorage/Box-Box/safewatertime/data/Country_Population_MICS.xlsx")
pop <- pop[, 1:3]

pop <- pop %>%
  mutate(
    population = case_when(
      grepl("m$", population) ~ as.numeric(sub("m$", "", population)) * 10^6,  # Replace 'm' and multiply
      !grepl("m$", population) & !is.na(population) ~ as.numeric(population),   # Convert other numeric values
      TRUE ~ NA_real_  # Handle cases where population is not numeric or NA
    )
  )

merged_data <- fuzzy_left_join(
  merged_data,
  pop,
  by = c("country" = "country", "Community Type" = "Community Type"),
  match_fun = list(
    # Fuzzy matching for 'country' using Levenshtein distance:
    function(x, y) stringdist::stringdist(x, y, method = "lv") <= 2,
    # Exact match for 'Community Type':
    `==`
  )
)

#merged_data <- merged_data %>%
#  left_join(pop, by = c("country", "Community Type")) 

merged_data <- merged_data %>%
  select(-country.y, -`Community Type.y`) %>%
  rename(
    country = country.x,
    `Community Type` = `Community Type.x`
  )

merged_data <- merged_data %>%
  mutate(
    Population = as.numeric(as.character(Population)),
    population = as.numeric(population),
    # Replace NA in Population with the population value from pop
    Population = if_else(is.na(Population), population, Population)
  ) %>%
  select(-population)



#write.csv(merged_data, paste0(here::here(), "/1-dm/DHS_MICS_Dec_3.csv"))