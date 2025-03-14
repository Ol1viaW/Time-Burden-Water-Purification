
MICS_years <- read_excel("~/Downloads/Tracking just MICS-4.xlsx")
MICS_years <- MICS_years %>%
  mutate(`Last MICS year` = ifelse(grepl("-", `Last MICS year`), 
                                   sub(".*-(\\d{4})", "\\1", `Last MICS year`), 
                                   ifelse(grepl("\\.0$", `Last MICS year`), 
                                          sub("\\.0$", "", `Last MICS year`), 
                                          `Last MICS year`)))
MICS_years <- MICS_years %>%
  arrange(`MICS VersioN`)

MICS_years$Country[MICS_years$Country == "Vietnam"] <- "Viet_Nam"

##MAKE SURE TO RUN MICS6_All_Countries_T, MICS5_All_Countries_T, MICS4_All_Countries_T, MICS3_All_Countries_T,
##MICS6_All_Countries_UR, MICS5_All_Countries_UR, MICS4_All_Countries_UR, MICS3_All_Countries_UR
#MICS6_T

combined_data <- rbind(MICS6_UR, MICS6_T, MICS5_UR, MICS5_T, MICS4_UR, MICS4_T, MICS3_UR, MICS3_T)
sorted_data <- combined_data %>%
  arrange(country)

MICS <- sorted_data %>%
  left_join(MICS_years %>% select(Country, `Last MICS year`), by = c("country" = "Country")) %>%
  rename(Year = `Last MICS year`)

MICS <- MICS %>%
  mutate(Type = 'MICS')  %>%
  mutate(`Year` = as.numeric(`Year`)) 


MICS <- MICS %>%
  rename(
    `Community Type` = Community_Type,
    `Treats water with acceptable method` = Effective_water_treatment,
    `Female Under 15` = Female_Under_15,
    `Female Over 15` = Female_Over_15,
    `Female Any Age` = Female_Any_Age,
    `Piped onto premises` = Piped_onto_premises,
    SurveyYear = Year
  )


country_counts <- table(MICS$country)

country_counts <- table(MICS$country)

# Get countries that appear exactly 3 times
exactly_three_times <- names(country_counts[country_counts == 3])

# Get countries that do not appear exactly 3 times
not_exactly_three <- names(country_counts[country_counts != 3])

# Display results
cat("Countries that appear exactly 3 times:\n")
print(exactly_three_times)

cat("\nCountries that do not appear exactly 3 times:\n")
print(not_exactly_three)

# Check if 'not_exactly_three' exists and print it
if (exists("not_exactly_three")) {
  cat("\nCountries that do not appear exactly 3 times:\n")
  print(not_exactly_three)
} else {
  cat("No countries found that do not appear exactly 3 times.\n")
}

#write_xlsx(MICS, path = "MICS_March_3.xlsx") 