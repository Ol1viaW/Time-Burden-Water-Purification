#download DHS_dta folder from BOX and set up path for paste0 dhs_dta
#droping Peru, PE because don't specify male/female

countryIds = c("AL","AO","AM","AZ","BO",
               "BF","BU", "KH", "CI", "EG", 
               "GA", "GM", "GH", "GU", "HT", 
               "IA", "ID", "JO", "KE", "KY", 
               "MD", "MV", "MR","MM", "NM",
               "NP", "PE", "PH", 
               "RW", "SL", "ZA", "TJ", "TZ", 
               "TL","ZM")
countryIds = c("NP", "PE", "PH", 
               "RW", "SL", "ZA", "TJ", "TZ", 
               "TL","ZM")
#PG
#PK
#NG
#38 countries
#DHS IDs for selected countries
# Initialize an empty data frame to store the results
DHS_df1 = data.frame()
# Loop through each country ID
for (i in countryIds) {
  file_PR <- paste0(pr_dta, "/", i, "PR", ".DTA")
  i_PR <- read_dta(file_PR)
  colnames(i_PR) <- trimws(colnames(i_PR))
  
  if (i == 'BO') {
    # Directly selecting columns by names since 'BO' case is specified
    i_PR_w <- i_PR %>%
      select(hv005, hv021, hv025, hv237a, hv237b, hv237c, hv237d, hv237e, hv237f)
    new_column_names <- c("weight", "PSU", "type", "Boil", "Bleach", "Strain", "Filter", "Solar", "LetStand")
    colnames(i_PR_w) <- new_column_names
  } else {
    water_labels_PR <- c(
      "household sample weight (6 decimals)",
      "primary sampling unit", 
      "type of place of residence", 
      "water usually treated by: boil",
      "water usually treated by: add bleach/chlorine",
      "water usually treated by: strain through a cloth",
      "water usually treated by: use water filter",
      "water usually treated by: solar disinfection",
      "water usually treated by: let it stand and settle"
    )
    
    select_by_labels <- function(dataframe, labels) {
      column_names <- vector("character", length(labels))
      
      for (i in seq_along(labels)) {
        label <- labels[i]
        matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == label)]
        
        if (length(matched_col) == 0 && label == "line number person fetching water") { 
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl)
            return(any(lbl %in% c("na - person fetching water", "person fetching water", "Person fetching water", "Person collecting water", "Qui se rend habituellement a la source d'eau", "Personne habituellement chargée de chercher de l'eau", "Qui va chercher principalement de l'eau", "Qui va chercher de l'eau", "Persona quien recolecta agua", 
                                  "Perfil del responsable de la recolección de agua", "Pessoa, para apanahar água", "Qui va habituellement à la source pour collecter de l'eau pour votre ménage ?", "Personne qui va chercher l'eau habituellement",
                                  "Personne qui collecte de l'eau", "Qui va chercher principalement de l'eau", "Persona que recoge agua", "Persona que recoge agua habitualmento en el hogar", "Persona que recoge agua habitualmente en el hogar",
                                  "Quem se desloca habitualmente para a fonte de aprovisionamento para ir buscar água para AF?", "Qui va habituellement à la source pour collecter de l’eau pour votre ménage",
                                  "Numéro de ligne de la personne qui va à la source pour collecter l'eau", "¿Quién va habitualmente a ese lugar a buscar el agua para su hogar?")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "primary sampling unit") { 
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("PSU")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "type of place of residence") { 
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("residence")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "water usually treated by: boil") { #WS10A
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("na - water usually treated by: boil")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "water usually treated by: add bleach/chlorine") { 
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("na - water usually treated by: add bleach/chlorine")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "water usually treated by: strain through a cloth") { #WS10B
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("na - water usually treated by: strain through a cloth")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "water usually treated by: use water filter") { 
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("na - water usually treated by: use water filter"
            )))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "water usually treated by: solar disinfection") {
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("na - water usually treated by: solar disinfection")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
        }
        
        if (length(matched_col) == 0 && label == "water usually treated by: let it stand and settle") { 
          matched_col <- names(dataframe)[sapply(dataframe, function(col) {
            lbl <- var_label(col)
            if (is.null(lbl)) return(FALSE)
            lbl <- unlist(lbl) #making sure format matches
            return(any(lbl %in% c("na - water usually treated by: let it stand and settle")))
          })]
          if (length(matched_col) > 0) {
            matched_col <- matched_col[1]
            column_names[i] <- matched_col
          }
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
    #rm(i_PR)
    #gc()
    #hv025 type of place of residence
    #hv023 stratification
    #hv022 sample strata
    #hv021 primary sampling unit
    #hv005 household sample weight
    #hv002 household number
    #hv001 cluster number
    #hvidx line number
    new_column_names <- c("weight", "PSU", "type", "Boil", "Bleach", "Strain", "Filter", "Solar", "LetStand")
    new_column_names <- as.character(new_column_names)
    if(any(is.na(new_column_names))) {
      print('na')
      print(i)
    }
    if(length(new_column_names) == ncol(i_PR_w)) {
      names(i_PR_w) <- new_column_names
    } else {
      warning("Mismatch between column names and columns in i_PR_w")
      print(i)
    }
    
    if((ncol(i_PR_w) < 9)) {
      print("PROBLEM")
      print(i)
    }
  }
  rm(i_PR)
  gc()
  
  if((ncol(i_PR_w) == 9)) {
    
    i_PR_w$water_treatment_sum <- rowSums(i_PR_w[, c("Boil", "Bleach", "Strain", "Filter", "Solar")], na.rm = TRUE)
    i_PR_w$Multiple_water_treatment <- ifelse(
      i_PR_w$water_treatment_sum > 1, 
      1, 0)
    
    variables <- c("Boil", "Bleach", "Strain", "Filter", "Solar", "LetStand", "Multiple_water_treatment"
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    for (var in variables) {
      # Update the variable names in the survey design and calculations
      
      dta <- i_PR_w
      dta$wt <- dta$weight
      
      # Create survey design object
      mysurvey <- svydesign(id = ~PSU, data = dta, strata = ~type,
                            weight = ~wt, nest = TRUE)
      # Adjust options for survey package
      options(survey.lonely.psu = "adjust")
      
      # Calculate weighted frequencies
      if (var == "Boil") {
        weighted_freqs <- svytable(~Boil + type, mysurvey) 
      }
      else {
        weighted_freqs <- svytable(as.formula(paste0("~", var, " + type")), mysurvey)
      }
      
      #########
      #mysurvey <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
      #                     weight = ~wt, nest = TRUE)
      
      # Adjust options for survey package
      #options(survey.lonely.psu = "adjust")
      
      #i_weighted_freqs_over_15 <- svytable(~i_water_over_15 + hh6, mysurvey)
      
      #########
      # Calculate proportions
      proportion <- prop.table(weighted_freqs, margin = 2)
      proportions <- proportion * 100 #want to turn into percent
      
      if (!all(is.na(i_PR_w[[var]]))) {
        table_var <- as.data.frame(proportions) %>%
          filter(!is.na(!!sym(var)) & !!sym(var) != 0) %>%
          rename(
            country = !!sym(var),
            Community_Type = type
          ) %>%
          rename_with(~ if_else(. == "Freq", var, .))
        
        # Filter for country == 1, recode 'type' as 'urban'/'rural'
        table_var <- table_var %>%
          filter(country == 1)
        
        if (nrow(table_var) == 0) {
          table_var <- data.frame(
            country = i,
            Community_Type = c("Urban", "Rural")
          )
          table_var[[var]] <- 0.00
        } else {
          table_var <- table_var %>%
            mutate(
              Community_Type = case_when(
                Community_Type == 1 ~ 'Urban',
                Community_Type == 2 ~ 'Rural',
                TRUE ~ as.character(Community_Type)  # Handle cases where neither 1 nor 2
              )
            )
          table_var$country <- i
        }
      } else {
        # Handle the case where var is entirely NA
        table_var <- data.frame(
          country = i,
          Community_Type = c("Urban", "Rural")
        )
        table_var[[var]] <- NA
      }
      
      if (var != 'Boil') {
        combined_results <- left_join(combined_results, table_var, by = c("country", "Community_Type"))
      }
      else {
        results_list[[var]] <- table_var
        combined_results <- bind_rows(results_list)
      }
    }
    rm(i_PR_w)
    gc()
    print(i)
    DHS_df1 <- rbind(DHS_df1, combined_results)
  }
}
print('hi')