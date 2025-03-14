#download DHS_dta folder from BOX and set up path for paste0 dhs_dta
#droping Peru, PE because don't specify male/female
countryIds = c("AO", "AL","AO","AM","AZ","BO",
               "BF","BU", "KH", "CI", "EG", 
               "GA", "GM", "GH", "GU", "HT", 
               "IA", "ID", "JO", "KE", "KY", 
               "MD", "MV", "MR","MM", "NM", "NG",
               "NP", "PK", "PG", "PH", 
               "RW", "SL", "ZA", "TJ", "TZ", 
               "TL","ZM")

countryIds2 = c("AO", "BF")
#38 countries
#DHS IDs for selected countries
# Initialize an empty data frame to store the results
df = data.frame()
# Loop through each country ID
for (i in countryIds) {
  file_PR <- paste0(pr_dta, "/", i, "PR", ".DTA")
  i_PR <- read_dta(file_PR)
  colnames(i_PR) <- trimws(colnames(i_PR))
  
  water_labels_PR <- c(
    "line number", 
    "cluster number", 
    "household number", 
    "household sample weight (6 decimals)",
    "primary sampling unit", 
    "sample strata for sampling errors",
    "stratification used in sample design",
    "type of place of residence", 
    "sex of household member",
    "age of household members",
    "line number person fetching water"
  )
  
  select_by_labels <- function(dataframe, labels) {
    column_names <- vector("character", length(labels))
    
    for (i in seq_along(labels)) {
      label <- labels[i]
      matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == label)]
      
      if (length(matched_col) == 0 && label == "line number person fetching water") {
        matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == "na - person fetching water")]
      }
      
      if (length(matched_col) == 0 && label == "household sample weight (6 decimals)") {
        matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == "sample weight")]
      }
      
      if (length(matched_col) > 0) {
        column_names[i] <- matched_col
      }
    }
    
    column_names <- column_names[column_names != ""]  # Remove empty elements
    
    if (length(column_names) == 0) {
      stop("No columns found with the specified labels.")
    }
    
    return(select(dataframe, all_of(column_names)))
  }
  
  i_PR_w <- select_by_labels(i_PR, water_labels_PR)
  
  new_column_names <- c("hvidx", "hv001", "hv002", "hv005", "hv021", "hv022", "hv023", "hv025", "hv104", "hv105", "hv236a")
  names(i_PR_w) <- new_column_names
  
  if (!all(is.na(i_PR_w$hv236a))) {
    i_PR_w$i_water <- ifelse(i_PR_w$hv236a == 0 | is.na(i_PR_w$hv236a), NA, 5)
    
    i_PR_water_check <- i_PR_w %>% filter(!is.na(i_water))
    i_PR_wateronly <- i_PR_w %>% filter(hv236a == hvidx)
    
    i_PR_wateronly <- i_PR_wateronly %>%
      mutate(
        i_water_all_ages = ifelse(is.na(hv104), NA, ifelse(hv104 == 2, 1, 0)),
        i_water_under_15 = ifelse(is.na(hv104) | is.na(hv105), NA, ifelse(hv104 == 2 & hv105 < 15, 1, 0)),
        i_water_over_15 = ifelse(is.na(hv104) | is.na(hv105), NA, ifelse(hv104 == 2 & hv105 >= 15, 1, 0))
      )
    
    #doesn't account for na
    #i_PR_wateronly$i_water_all_ages <- ifelse(i_PR_wateronly$hv104 == 2, 1, 0)
    
    #i_PR_wateronly$i_water_under_15 <- ifelse(i_PR_wateronly$hv104 == 2 & i_PR_wateronly$hv105 < 15, 1, 0)
    
    #i_PR_wateronly$i_water_over_15 <- ifelse(i_PR_wateronly$hv104 == 2 & i_PR_wateronly$hv105 >= 15, 1, 0)
    #no na values for sex and age
    #check 2 female and 1 male
    
    # #------------------------
    # #Weighting using PR
    # #------------------------
    #household weight variable is hv005, primary sampling unit is hv021 and the strata variable is hv025 since want stratification with Urban and rural split
    
    # Open the model dataset
    dta <- i_PR_wateronly
    
    # Create weight variable using household sample weight
    dta$wt <- dta$hv005 / 1000000
    
    # Create survey design object
    #hv021 is the primary sampling unit
    mysurvey_2 <- svydesign(id = ~hv021, data = dta, strata = ~hv025,
                            weight = ~wt, nest = TRUE)
    
    # Adjust options for survey package
    options(survey.lonely.psu = "adjust")
    
    # To get weighted frequencies for the question split by hv025 (Urban and rural)
    weighted_freqs_over_15 <- svytable(~i_water_over_15 + hv025, mysurvey_2)
    
    # To get proportions split by hv025
    proportions <- prop.table(weighted_freqs_over_15, margin = 2)
    
    # Store the proportions in table1
    i_table_over_15 <- proportions
    
    weighted_freqs_under_15 <- svytable(~i_water_under_15 + hv025, mysurvey_2)
    proportions_under_15 <- prop.table(weighted_freqs_under_15, margin = 2)
    i_table_under_15 <- proportions_under_15
    
    weighted_freqs_all_ages <- svytable(~i_water_all_ages + hv025, mysurvey_2)
    proportions_all_ages <- prop.table(weighted_freqs_all_ages, margin = 2)
    i_table_all_ages<- proportions_all_ages

    #droping rows with 0
    i_table_all_ages <- as.data.frame(i_table_all_ages)
    i_table_under_15 <- as.data.frame(i_table_under_15)
    i_table_over_15 <- as.data.frame(i_table_over_15)
    
    i_table_all_ages <- i_table_all_ages %>%
      filter(i_water_all_ages != 0) %>%
      rename(
        country = i_water_all_ages,  # Renaming new country code columns
        `Community_Type` = hv025,  
        `Female_Any_Age` = Freq  
      )
    i_table_under_15 <- i_table_under_15 %>%
      filter(i_water_under_15 != 0)  %>%
      rename(
        country = i_water_under_15,  # Renaming new country code columns
        `Community_Type` = hv025,  
        `Female_Under_15` = Freq  
      )
    i_table_over_15 <- i_table_over_15 %>%
      filter(i_water_over_15 != 0)  %>%
      rename(
        country = i_water_over_15,  # Renaming new country code columns
        `Community_Type` = hv025,  
        `Female_Over_15` = Freq  
      )
    
    #want the country codes
    i_table_all_ages$country <- i
    i_table_under_15$country <- i
    i_table_over_15$country <- i
    
    # Assign values to 'Community_Type' column based on values
    # 1 is Urban and 2 is rural
    i_table_all_ages$`Community_Type` <- ifelse(i_table_all_ages$`Community_Type` == 1, 'Urban', 'Rural')
    i_table_under_15$`Community_Type` <- ifelse(i_table_under_15$`Community_Type` == 1, 'Urban', 'Rural')
    i_table_over_15$`Community_Type` <- ifelse(i_table_over_15$`Community_Type` == 1, 'Urban', 'Rural')
    
    #1 is Urban and 2 is rural
    #for the table 1 is when the column (ex. age over 15) is true
    #mastertable
# Print the master table
    
    output_all_under <- left_join(i_table_all_ages, i_table_under_15, by = c("country", "Community_Type"))
    output  <- left_join(output_all_under, i_table_over_15, by = c("country", "Community_Type"))
    df = rbind(df, output)

  } else {
    
    i_table_na <- data.frame(
      country = rep(i, 2),
      Community_Type = c("Urban", "Rural"),
      Female_Any_Age = rep(NA, 2),
      Female_Under_15 = rep(NA, 2),
      Female_Over_15 = rep(NA, 2)
    )
    
    df = rbind(df, i_table_na)
  }
}

########
#-------
#Trying to get text attribute/text label
#-------
########

# Get all attributes of the column
#attributes(i_HR$hv237a$label)

# Get the variable label of the column
#variable_label <- var_label(i_HR$hv237a)
# water usually treated by: boil"

# Use the function to get the column by its label
#water_treated_column <- get_column_by_label(i_HR, "water usually treated by: boil")


# Define a function to select columns by their label
#select_by_label <- function(dataframe, label) {
#  column_name <- names(dataframe)[sapply(dataframe, function(col) labelled::var_label(col) == label)]
#  if (length(column_name) == 0) {
#    stop("No column found with the specified label.")
#  }
#  return(dplyr::select(dataframe, dplyr::all_of(column_name)))
#}

# Use the function to select the column by its label
#i_HR_w <- select_by_label(i_HR, "water usually treated by: boil")
#0 is no, 1 is yes, 8 is don't know

