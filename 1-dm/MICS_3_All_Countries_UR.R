#download DHS_dta folder from BOX and set up path for paste0 dhs_dta

countryIds_all = c("Albania", "Burundi", "Djibouti", "Mozambique", "Syria", "Tajikistan")
countryIds = c("Albania", "Tajikistan", "Djibouti", "Syria")

#Mozambique NA weight values
#38 countries
#DHS IDs for selected countries
# Initialize an empty data frame to store the results
MICS3_df1 = data.frame()
MICS3_df2 = data.frame()

#function to select columns by text label
select_by_labels <- function(dataframe, labels) {
  column_names <- vector("character", length(labels))
  
  for (i in seq_along(labels)) {
    label <- labels[i]
    matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == label)]
    
    if (length(matched_col) > 0) {
      # Select the first matched column
      matched_col <- matched_col[1]
      column_names[i] <- matched_col
    }
    
    if (length(matched_col) == 0 && label == "Cluster") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Numero de la grappe", "Numéro séquenetiel de la grappe", "Numéro de Grappe", "Numéro de la grappe", "Número de conglomerado", "Número de Conglomerado(UPM)",
                              "Cluster number", "Numero de la grappe", "Numéro séquenetiel de la grappe", "Numéro de Grappe","Numéro de grappe", "Numéro de la grappe", "Número de conglomerado", "Número de Conglomerado(UPM)", "Número de conglomerado o UPM", "Número do DR Nouvel ordre",
                              "Numéro de área de Enumeração", "Código de consultorio", "Número de área de Enumeração", "Area de Enumeração (MICS I.D)")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Household number") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Numero du menage", "Numéro du ménage", "Número de hogar", "Número de Hogar(Vivienda)", "Numero du menage", "Numéro du ménage", "Número de hogar", "Número de Hogar(Vivienda)", "Número de orden de la vivienda en la muestra", 
                              "Número do Agregado Familiar", "Numéro de AF", "Numéro de ménage", "Número de AF")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Main source of drinking water") { #WS1
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Main source of water", "Quelle est la source principale d'eau que boivent les membres de votre ménage", "Principale source d'eau de boisson",
                              "Source principale d'eau de boisson", "Principal fuente de agua para beber", "Source principale d'eau potable", "Principale source d'eau boivent les membre du menage", "Principale source d'eau que boivent les membre du menage",
                              "Qual a principal fonte de abastecimento de agua usada p", "WS1.Main source of drinking water")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Area") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Urban_Rural", "Milieu", "Milieu de résidence", "Milieu de Résidence", "Área", "Area of residence", "Mileu", "Urbano/Rural",
                              "HH6.Area")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Person fetching water") { #WS4
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Qui se rend habituellement a la source d'eau", "Personne habituellement chargée de chercher de l'eau", "Qui va chercher principalement de l'eau", "Qui va chercher de l'eau", "Persona quien recolecta agua", "Perfil del responsable de la recolección de agua",
                              "Person collecting water", "Qui se rend habituellement a la source d'eau", "Personne habituellement chargée de chercher de l'eau", "Qui va chercher principalement de l'eau", "Qui va chercher de l'eau", "Persona quien recolecta agua", 
                              "Perfil del responsable de la recolección de agua", "Pessoa, para apanahar água", "Qui va habituellement à la source pour collecter de l'eau pour votre ménage ?", "Personne qui va chercher l'eau habituellement",
                              "Personne qui collecte de l'eau", "Qui va chercher principalement de l'eau", "Persona que recoge agua", "Persona que recoge agua habitualmento en el hogar", "Persona que recoge agua habitualmente en el hogar",
                              "Perfil del responsable de la recolección de agua", "Quem é que normalmente vai a essa fonte buscar água par", "WS4.Person fetching water")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Treat water to make water safer to drink") { #WS5
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Treats water", "Treat water to make safer for drinking", "Faites-vous, ou n'importe quel autre membre de ce ménage, quelque chose à l'eau pour la rendre plus saine à boire",
                              "Eau traitée pour la rendre saine à boire", "Traiter l'eau pour la rendre plus saine à boire", "Traite l'eau avant de la boire", "El agua fue tratada para que sea más segura para beber",
                              "El agua fue tratada para que sea segura para beber", "Alguna forma para hacer el agua más segura para beber", "Traitez vous l'eau avant de la boire",
                              "Trata a sua água de alguma maneira para ela ficar segur", "WS5.Treat water to make safer for drinking")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Boil") { #WS6A
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Boil", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: LA FAIRE BOUILLLIR",
                              "Traitement de l'eau: Bouillir", "Traitement de l'eau : Bouillir", "Tratamiento de agua: hervir", "Tratamiento de agua: hervir",
                              "Water treatment: boil", "Tratamiento del agua: La hierve", "Tratamiento del agua: la hierve", "La faire bouillir", "Boil", "Ferver",
                              "WS6A.Boil")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Add bleach/chlorine") { #WS6B
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Add bleach/chlorine", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:AJOUTER DE LA JAVEL/ CHLORE",
                              "Traitement de l'eau: Ajout de javel/chlore", "Traitement de l'eau : Ajout de javel/chlore", "Traitement de l'eau : Ajouter de la javel/chlore",
                              "Traitement de l'eau: Ajouter de la javel/ chlore", "Tratamiento de agua: agrega hiclorito / chloro", "Water treatment: Add jikbleach/chlorine",
                              "Tratamiento de agua: agregue blanqueador / cloro", "Water treatment: Add bleach / chlorine", "Water treatment: add bleach/chlorine",
                              "Water treatment: Add bleach/Chlorine", "Traitement de l'eau : Ajouter de l'eau de javel/de chlore", "Tratamiento del agua: le añaden blaqueador", 
                              "Y ajouter de l'eau de javel/chlore", "Add bleach/chlorine", "Adicionar Lixivia/Cloro", "WS6B.Add bleach/chlorine")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Strain it through a cloth") { #WS6C
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: strain it through a cloth", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:FILTRER A TRAVERS UN TISSU",
                              "Traitement de l'eau: Filtrer avec un tissu", "Traitement de l'eau: Filtrer avec un tissu", "Traitement de l'eau : Filtrer avec un tissu", "Traitement de l'eau : Filtrer a travers un tissu",
                              "Traitement de l'eau: Filtrer a travers un tissu", "Tratamiento de agua: filtra con una tela", "Tratamiento de agua: filtra con una tela", "Tratamiento de agua: filtra con una tela",
                              "Water treatment: strain it through a cloth", "Traitement de l'eau : La filtrer à travers d'un linge", "Tratamiento del agua: la filtra con una tela", "La filtrer a travers un linge",
                              "Strain it through a cloth", "Filtrar com um pano", "WS6C.Strain it through a cloth"
        )))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Use water filter") { #WS6D
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Use water filter", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:UTILISER UN FILTRE A EAU (CERAMIQUE, SABLE, COMPOSITE, ETC.)",
                              "Traitement de l'eau: Utiliser un filtre à eau", "Traitement de l'eau : Utiliser un filtre à eau", "Tratamiento de agua: filtro de agua", "Water treatment: use water filter",
                              "Traitement de l'eau : Utiliser un filtre", "Tratamiento del agua: utiliza un filtro de agua", "Utiliser un filtre (ceramique, sable, composite)", "Utiliser un filtre  (ceramique, sable, composite)",
                              "Use water filter", "Usar água do filtro", "WS6D.Use water filter")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Solar disinfection") { #WS6E
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Solar disinfection", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: DESINFECTION SOLAIRE",
                              "Traitement de l'eau: Désinfection solaire", "Traitement de l'eau : Désinfection solaire", "Traitement de l'eau : Desinfection solaire",
                              "Tratamiento de agua: Desinfección solar", "Water treatment: Sunlight Exposure (Solar disinfection)", "Water treatment: solar disinfection", "Tratamiento del agua: Desinfección solar",
                              "Desinfection solaire", "Solar disinfection", "Desinfecção solar", "WS6E.Solar disinfection")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Let it stand settle") { #WS6F
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Let it stand and settle", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: LA LAISSER REPOSER ET DECANTER", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: LA LAISSER REPOSER ET DECANTER",
                              "Traitement de l'eau: Laisser reposer et décanter", "Traitement de l'eau : Laisser reposer et décanter", "Traitement de l'eau : Laisser réposer et décanter", "Traitement de l'eau : La laisser reposer et decanto", "Tratamiento de agua: dejarla reposar y asentarse", 
                              "Tratamiento de agua: dejarlo reposar y asentarse", "Water treatment: Let it stand and settle", "Water treatment: let it stand and settle", "Tratamiento del agua: la deja reposar y asentar",
                              "Traitement de l'eau : La laisser et reposer", "Water treatment:Let it stand and settle", "Laisser reposer", "Let it stand and settle", "Deixar repousar e assentar",  "WS6F.Let it stand and settle")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Other") { #WS6X
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Other", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: AUTRE (Préciser)",
                              "Traitement de l'eau: Autre", "Traitement de l'eau : Autre", "Tratamiento de agua: otro", "Water treatment: other",
                              "Tratamiento del agua: Otros", "Autres")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: DK") { #WS6Z
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: DK", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:NSP",
                              "Traitement de l'eau: NSP", "Traitement de l'eau : NSP", "Tratamiento de agua: No sabe", "Tratamiento del agua: No sabe", "NSP" )))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Household weight") { 
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Household sample weight", "Poids de ponderation menage", "Poids échantillon ménages", "Household Weight", "Household weight", "Ponderación del hogar",
                              "Household sample weight", "Poids de ponderation menage", "Poids échantillon ménages", "Household Weight", "Weights", "Ponderación del hogar",
                              "Ponderadores para agregados familiares", "Ponderador de amostra agregados familiares", "Poids d'échantillonnage du ménage", "Poids de l'échantillon du ménage",
                              "Poids de l'échantillon des ménages", "Peso de muestra del hogar", "Ponderador de hogares", "Factor ponderación del hogar", "Pesos muestral del hogar",
                              "Ponderador de amostra agregados familiares", "Sample weight - household")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Time to get water and come back") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == "time to get water and come back")]
      if (length(matched_col) == 0) {
        matched_col <- names(dataframe)[sapply(dataframe, function(col) var_label(col) == "Temps pour aller chercher de leau et revenir")]
      }
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
  }
  
  column_names <- column_names[column_names != ""]  # Remove empty elements
  
  if (length(column_names) == 0) {
    stop("No columns found with the specified labels.")
  }
  
  return(select(dataframe, all_of(column_names)))
}
# Loop through each country ID
for (i in countryIds) {
  file_hh <- paste0(mics3_dta, "/", i, "/", "hh", ".sav")
  i_hh <- read_sav(file_hh)
  colnames(i_hh) <- trimws(colnames(i_hh))
  
  if (i == "Burundi") {
    water_labels_hh <- c(
      "Cluster", 
      "Household number", 
      "Mileu",
      "Main source of drinking water",
      "Water treatment: Boil", #WS6A
      "Water treatment: Add bleach/chlorine", #WS6B
      "Water treatment: Strain it through a cloth", #WS6C
      "Water treatment: Use water filter", #WS6D
      "Water treatment: Solar disinfection", #WS6E
      "Water treatment: Let it stand settle", #WS6F
      "Water treatment: Other", #WS6X
      "Water treatment: DK", #WS6Z
      "Femme adulte",
      "Homme adulte",
      "Fille de moins de 15 ans",
      "Garçon de moins de 15 ans",
      "Pondération ménage"
    )
   
    i_hh_w <- select_by_labels(i_hh, water_labels_hh)
    new_column_names <- c("hh1", "hh2", "hh6", "WS1", "WS10A", "WS10B", "WS10C", "WS10D", "WS10E", "WS10F", "WS10X", "WS10Z",
                          "ws4A", "ws4B",  "ws4C",  "ws4D", "hhweight")
    names(i_hh_w) <- new_column_names
    
    ##------
    #Code for non-water fetching questions 
    ##-----
    #using combined_df create new column called Piped onto premises which is 1 if columns WS1 is 11 or 12 and 0 if 13,14,21,31,32,41,42,51,61,71, 72,81,91,92,96
    i_hh_w <- i_hh_w %>%
      mutate(Piped_onto_premises = case_when(
        is.na(WS1) ~ NA,
        WS1 %in% c(11, 12) ~ 1,
        !(WS1 %in% c(11, 12)) ~ 0
      ))
    
    i_hh_w <- i_hh_w %>%
      mutate(Any_water_treatment = case_when(
        WS10A == "A" ~ 1,
        WS10B == "B" ~ 1,
        WS10C == "C" ~ 1,
        WS10D == "D" ~ 1,
        WS10E == "E" ~ 1,
        WS10F == "F" ~ 1,
        WS10X == "X" ~ 1,
        WS10Z == "Z" ~ 0,  # 'Don't know' is treated as a 'No'
        TRUE ~ 0           # Default case (else condition)
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Boil = case_when(
        WS10A == "A" ~ 1,
        (WS10A != "A") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Bleach = case_when(
        WS10B == "B" ~ 1,
        (WS10B != "B") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Strain = case_when(
        WS10C == "C" ~ 1,
        (WS10C != "C") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Filter = case_when(
        WS10D == "D" ~ 1,
        (WS10D != "D") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(SolarDisinfection = case_when(
        WS10E == "E" ~ 1,
        (WS10E != "E") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(LetStand = case_when(
        WS10F == "F" ~ 1,
        (WS10F != "F") ~ 0,
      ))
    
    i_hh_w$Effective_water_treatment <- ifelse(
      (i_hh_w$Boil == 1 | i_hh_w$Bleach == 1 | i_hh_w$Filter == 1 | i_hh_w$SolarDisinfection == 1), 
      1, 
      0
    )
    
    i_hh_w$water_treatment_sum <- rowSums(i_hh_w[, c("Boil", "Bleach", "Strain", "Filter", "SolarDisinfection")], na.rm = TRUE)
    i_hh_w$Multiple_water_treatment <- ifelse(
      i_hh_w$water_treatment_sum > 1, 
      1, 0)
    
    #remove the intermediate sum column
    #combined_df$water_treatment_sum <- NULL
    
    #WS9 yes is 1 to water treatment, no is 2, DK is 8 (if don't know MICS6 logic skips the question specifying water treatment)
    
    #  "Main source of drinking water", #WS1
    #"Treat water to make water safer to drink", #WS9
    #Water treatment: Boil", #WS10A
    #Water treatment: Add bleach/chlorine", #WS10B
    #  "Water treatment: Strain it through a cloth", #WS10C
    # "Water treatment: Use water filter", #WS10D
    #"Water treatment: Solar disinfection", #WS10E
    #"Water treatment: Let it stand settle" #WS10F
    
    # List of variables to loop over
    
    variables <- c("Piped_onto_premises", "Boil", "Bleach", "Strain", "Filter", "SolarDisinfection", "LetStand", "Any_water_treatment", "Effective_water_treatment", "Multiple_water_treatment"
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    for (var in variables) {
      # Update the variable names in the survey design and calculations
      
      dta <- i_hh_w
      dta$wt <- dta$hhweight
      
      # Create survey design object
      mysurvey <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
                             weight = ~wt, nest = TRUE)
    
      # Adjust options for survey package
      options(survey.lonely.psu = "adjust")
      
      # Calculate weighted frequencies
      if (var == "boil") {
        weighted_freqs <- svytable(~Boil + hh6, mysurvey) 
      }
      else {
        weighted_freqs <- svytable(as.formula(paste0("~", var, " + hh6")), mysurvey)
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
      
      
      # Convert proportions to a data frame and filter out rows with 0
      if (var == 'Any_water_treatment'){
        table_var <- as.data.frame(proportions) %>%
          #filter(!!sym(var) != 8) %>%
          filter(!!sym(var) != 0) %>%
          rename(
            country = !!sym(var),
            Community_Type = hh6
          ) %>%
          rename_with(~ if_else(. == "Freq", var, .))
      }
      #8 is don't know for any water treatment
      if (var != 'Any_water_treatment'){
        table_var <- as.data.frame(proportions) %>%
          filter(!!sym(var) != 0) %>%
          rename(
            country = !!sym(var),
            Community_Type = hh6
          ) %>%
          rename_with(~ if_else(. == "Freq", var, .))
      }
      if (nrow(table_var) == 0) {
        # Create a table_var with default values if no rows are present
        table_var <- data.frame(
          country = i,
          Community_Type = c("Urban", "Rural")
        )
        
        # Add the new column with NA values
        table_var[[var]] <- NA
      } else {
        table_var$country <- i
        # Modify `Community_Type` based on the number of rows
        table_var <- table_var %>%
          mutate(
            Community_Type = case_when(
              nrow(table_var) < 3 ~ case_when(
                Community_Type == 1 ~ 'Urban',
                TRUE ~ 'Rural'
              ),
              nrow(table_var) == 3 ~ case_when(
                Community_Type == 1 ~ 'Urban',
                Community_Type == 2 ~ 'Rural',
                Community_Type == 3 ~ 'Other'
              ),
              TRUE ~ Community_Type  # No change if more than 3 rows
            )
          )
      }
      if (var != 'Piped_onto_premises') {
        combined_results <- left_join(combined_results, table_var, by = c("country", "Community_Type"))
      }
      else {
        results_list[[var]] <- table_var
        combined_results <- bind_rows(results_list)
      }
      
      if (var == "Multiple_water_treatment") {
        MICS3_df2 <- rbind(MICS3_df2, combined_results)
      }
    }
    
    ##------
    #End code for non-water fetching questions 
    ##-----
    
    
    
    i_hh_wateronly <- i_hh_w %>%
      mutate(
        i_water_all_ages = ifelse(ws4A == "A" | ws4C == "C", 1, 0),
        i_water_only_under_15 = ifelse(ws4A != "A" & ws4C == "C", 1, 0),
        i_water_only_over_15 = ifelse(ws4A == "A" & ws4C != "C", 1, 0)
      )
    dta <- i_hh_wateronly
    dta$wt <- dta$hhweight
    
    # Create survey design object
    mysurvey <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
                          weight = ~wt, nest = TRUE)
    
    # Adjust options for survey package
    options(survey.lonely.psu = "adjust")
    
    i_weighted_freqs_only_over_15 <- svytable(~i_water_only_over_15 + hh6, mysurvey)
    i_weighted_freqs_only_under_15 <- svytable(~i_water_only_under_15 + hh6, mysurvey)
    i_weighted_freqs_all_ages <- svytable(~i_water_all_ages + hh6, mysurvey)

    i_proportion_over_15 <- prop.table(i_weighted_freqs_only_over_15, margin = 2)
    i_proportion_under_15 <- prop.table(i_weighted_freqs_only_under_15, margin = 2)
    i_proportion_all_ages <- prop.table(i_weighted_freqs_all_ages, margin = 2)
    
    i_proportions_over_15 <- i_proportion_over_15 * 100 #want to turn into percent
    i_proportions_under_15 <- i_proportion_under_15 * 100 #want to turn into percent
    i_proportions_all_ages <- i_proportion_all_ages * 100 #want to turn into percent
    
    # Store proportions in i_table_over_15
    i_table_only_over_15 <- i_proportions_over_15
    i_table_only_under_15 <- i_proportions_under_15
    i_table_all_ages <- i_proportions_all_ages
    
    # Filter out rows with 0
    i_table_all_ages <- as.data.frame(i_table_all_ages)
    i_table_only_under_15 <- as.data.frame(i_table_only_under_15)
    i_table_only_over_15 <- as.data.frame(i_table_only_over_15)
    
    i_table_all_ages <- i_table_all_ages %>%
      filter(i_water_all_ages != 0) %>%
      rename(
        country = i_water_all_ages,
        `Community_Type` = hh6,
        `Female_Any_Age` = Freq
      )
    
    i_table_only_under_15 <- i_table_only_under_15 %>%
      filter(i_water_only_under_15 != 0) %>%
      rename(
        country = i_water_only_under_15,
        `Community_Type` = hh6,
        `Female_Under_15` = Freq
      )
    
    i_table_only_over_15 <- i_table_only_over_15 %>%
      filter(i_water_only_over_15 != 0) %>%
      rename(
        country = i_water_only_over_15,
        `Community_Type` = hh6,
        `Female_Over_15` = Freq
      )
    
    # Assign country codes
    i_table_all_ages$country <- i
    i_table_only_under_15$country <- i
    i_table_only_over_15$country <- i
    
    # Assign Community_Type based on values
    i_table_all_ages$`Community_Type` <- ifelse(i_table_all_ages$`Community_Type` == 1, 'Urban', 'Rural')
    i_table_only_under_15$`Community_Type` <- ifelse(i_table_only_under_15$`Community_Type` == 1, 'Urban', 'Rural')
    i_table_only_over_15$`Community_Type` <- ifelse(i_table_only_over_15$`Community_Type` == 1, 'Urban', 'Rural')
    
    # Combine tables
    output_all_under <- left_join(i_table_all_ages, i_table_only_under_15, by = c("country", "Community_Type"))
    output <- left_join(output_all_under, i_table_only_over_15, by = c("country", "Community_Type"))
    
    # Append to df
    MICS3_df1 <- rbind(MICS3_df1, output)
    
  }
  else if (i != "Burundi") {
    
    MICS3_water_labels_hh <- c(
      "Cluster", #hh1
      "Household number", #hh2
      "Area", #hh6
      "Main source of drinking water", #WS1
      "Person fetching water", #WS5
      "Treat water to make water safer to drink", 
      "Water treatment: Boil", #WS6A
      "Water treatment: Add bleach/chlorine", #WS6B
      "Water treatment: Strain it through a cloth", #WS6C
      "Water treatment: Use water filter", #WS6D
      "Water treatment: Solar disinfection", #WS6E
      "Water treatment: Let it stand settle", #WS6F
      "Household weight" #hhweight
    )
    #13 columns
   
    i_hh_w <- select_by_labels(i_hh, MICS3_water_labels_hh)
    
    new_column_names <- c("hh1", "hh2", "hh6", "WS1", "WS5", "WS9", "WS10A", "WS10B", "WS10C", "WS10D", "WS10E", "WS10F", "hhweight")
    #renumbering to match MICS6 WS10 for water treatment questions to be consistent
    
    names(i_hh_w) <- new_column_names
    
    ##------
    #Code for non-water fetching questions 
    ##-----
    #using combined_df create new column called Piped onto premises which is 1 if columns WS1 is 11 or 12 and 0 if 13,14,21,31,32,41,42,51,61,71, 72,81,91,92,96
    i_hh_w <- i_hh_w %>%
      mutate(Piped_onto_premises = case_when(
        is.na(WS1) ~ NA,
        WS1 %in% c(11, 12) ~ 1,
        !(WS1 %in% c(11, 12)) ~ 0
      ))
    
    i_hh_w <- i_hh_w %>%
      mutate(Any_water_treatment = case_when(
        is.na(WS9) ~ NA,
        WS9 == 1 ~ 1,
        WS9 == 2 ~ 0,
        WS9 == 8 ~ 0, #don't know considering equivalent to a no
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Boil = case_when(
        WS10A == "A" ~ 1,
        (WS10A != "A") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Bleach = case_when(
        WS10B == "B" ~ 1,
        (WS10B != "B") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Strain = case_when(
        WS10C == "C" ~ 1,
        (WS10C != "C") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(Filter = case_when(
        WS10D == "D" ~ 1,
        (WS10D != "D") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(SolarDisinfection = case_when(
        WS10E == "E" ~ 1,
        (WS10E != "E") ~ 0,
      ))
    
    i_hh_w  <- i_hh_w %>%
      mutate(LetStand = case_when(
        WS10F == "F" ~ 1,
        (WS10F != "F") ~ 0,
      ))
    
    i_hh_w$Effective_water_treatment <- ifelse(
      i_hh_w$Any_water_treatment == 1 & 
        (i_hh_w$Boil != 1 & i_hh_w$Bleach != 1 & i_hh_w$Filter != 1 & i_hh_w$SolarDisinfection != 1), 
      0,
      ifelse(
        i_hh_w$Boil == 1 | i_hh_w$Bleach == 1 | i_hh_w$Filter == 1 | i_hh_w$SolarDisinfection == 1, 
        1, 
        NA
      )
    )
    
    i_hh_w$water_treatment_sum <- rowSums(i_hh_w[, c("Boil", "Bleach", "Strain", "Filter", "SolarDisinfection")], na.rm = TRUE)
    i_hh_w$Multiple_water_treatment <- ifelse(
      i_hh_w$water_treatment_sum > 1, 
      1, 0)
    
    #remove the intermediate sum column
    #combined_df$water_treatment_sum <- NULL
    
    #WS9 yes is 1 to water treatment, no is 2, DK is 8 (if don't know MICS6 logic skips the question specifying water treatment)
    
    #  "Main source of drinking water", #WS1
    #"Treat water to make water safer to drink", #WS9
    #Water treatment: Boil", #WS10A
    #Water treatment: Add bleach/chlorine", #WS10B
    #  "Water treatment: Strain it through a cloth", #WS10C
    # "Water treatment: Use water filter", #WS10D
    #"Water treatment: Solar disinfection", #WS10E
    #"Water treatment: Let it stand settle" #WS10F
    
    # List of variables to loop over
    
    variables <- c("Piped_onto_premises", "Boil", "Bleach", "Strain", "Filter", "SolarDisinfection", "LetStand", "Any_water_treatment", "Effective_water_treatment", "Multiple_water_treatment"
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    for (var in variables) {
      # Update the variable names in the survey design and calculations
      
      dta <- i_hh_w
      dta$wt <- dta$hhweight
      
      # Create survey design object
      mysurvey2 <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
                             weight = ~wt, nest = TRUE)
      mysurvey <- svydesign(id = ~hh1, data = dta, strata = NULL,
                            weight = ~wt, nest = TRUE)
      
      # Adjust options for survey package
      options(survey.lonely.psu = "adjust")
      
      # Calculate weighted frequencies
      if (var == "boil") {
        weighted_freqs <- svytable(~Boil + hh6, mysurvey) 
      }
      else {
        weighted_freqs <- svytable(as.formula(paste0("~", var, " + hh6")), mysurvey)
      }
      
      #########
      #mysurvey <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
      #                     weight = ~wt, nest = TRUE)
      
      # Adjust options for survey package
      #options(survey.lonely.psu = "adjust")
      
      #i_weighted_freqs_over_15 <- svytable(~i_water_over_15 + hh6, mysurvey)
      
      #########
      # Calculate proportions
      proportion <- prop.table(weighted_freqs)
      proportions <- proportion * 100 #want to turn into percent
      
      
      # Convert proportions to a data frame and filter out rows with 0
      if (var == 'Any_water_treatment'){
        table_var <- as.data.frame(proportions) %>%
          #filter(!!sym(var) != 8) %>%
          filter(!!sym(var) != 0) %>%
          rename(
            country = !!sym(var),
            Community_Type = hh6
          ) %>%
          rename_with(~ if_else(. == "Freq", var, .))
      }
      #8 is don't know for any water treatment
      if (var != 'Any_water_treatment'){
        table_var <- as.data.frame(proportions) %>%
          filter(!!sym(var) != 0) %>%
          rename(
            country = !!sym(var),
            Community_Type = hh6
          ) %>%
          rename_with(~ if_else(. == "Freq", var, .))
      }
      if (nrow(table_var) == 0) {
        # Create a table_var with default values if no rows are present
        table_var <- data.frame(
          country = i,
          Community_Type = c("Urban", "Rural")
        )
        
        # Add the new column with NA values
        table_var[[var]] <- NA
      } else {
        table_var$country <- i
        # Modify `Community_Type` based on the number of rows
        table_var <- table_var %>%
          mutate(
            Community_Type = case_when(
              nrow(table_var) < 3 ~ case_when(
                Community_Type == 1 ~ 'Urban',
                TRUE ~ 'Rural'
              ),
              nrow(table_var) == 3 ~ case_when(
                Community_Type == 1 ~ 'Urban',
                Community_Type == 2 ~ 'Rural',
                Community_Type == 3 ~ 'Other'
              ),
              TRUE ~ Community_Type  # No change if more than 3 rows
            )
          )
      }
      if (var != 'Piped_onto_premises') {
        combined_results <- left_join(combined_results, table_var, by = c("country", "Community_Type"))
      }
      else {
        results_list[[var]] <- table_var
        combined_results <- bind_rows(results_list)
      }
      
      if (var == "Multiple_water_treatment") {
        MICS3_df2 <- rbind(MICS3_df2, combined_results)
      }
    }
    
    ##------
    #End code for non-water fetching questions 
    ##-----
    
    
  
  #hhweight sample weight of hh
  #using cluster # as primary sampling unit
  #hh1 (cluster #), hh2 (hh #), hl1 (line #), hh6 (Area -> 1 urban/ 2 rural), ws5 person line number of person fetching water
  #ws1 (main source of drinking water), ws3 (time to get water and come back)
  ##ws5 1 => adult woman, 3 => female child (under 15), 8 => DK, if water on premises will skip this question
  
  # Check if ws5 column has any non-NA values
  if (!all(is.na(i_hh_w$WS5)) && ncol(i_hh_w) > 12) {
    # If both conditions are true, execute this block
    
    # Drop rows where WS5 is NA
    i_hh_wa <- na.omit(i_hh_w)
    #drop rows where WS5 is 8 (don't know) or 9 missing
    #if (any(i_hh_wa$WS5 %in% c(8, 9))) {
    # i_hh_wa <- i_hh_wa[!(i_hh_wa$WS5 %in% c(8, 9)), ]
    #} 
    
    
    # Create new columns based on conditions and handle NA values
    i_hh_wateronly <- i_hh_wa %>%
      mutate(
        i_water_all_ages = ifelse(WS5 == 1 | WS5 == 3, 1, 0),
        i_water_under_15 = ifelse(WS5 == 3, 1, 0),
        i_water_over_15 = ifelse(WS5 == 1, 1, 0),
      )
    
    # Weighting using household sample weight
    dta <- i_hh_wateronly
    dta$wt <- dta$hhweight
    
    # Create survey design object
    mysurvey <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
                            weight = ~wt, nest = TRUE)
    
    # Adjust options for survey package
    options(survey.lonely.psu = "adjust")
    
    sum_water_U15 <- sum(dta$i_water_under_15, na.rm = TRUE)
    
    if (sum_water_U15 < 1) {
      i_table_under_15 <- data.frame(
        country = rep(i, 2),
        Community_Type = c("Urban", "Rural"),
        Female_Under_15 = rep(0.00, 2)
      )
      } else {
        
      i_weighted_freqs_under_15 <- svytable(~i_water_under_15 + hh6, mysurvey)
      i_proportions_under_15 <- prop.table(i_weighted_freqs_under_15, margin = 2)
      i_table_under_15 <- i_proportions_under_15
      i_table_under_15 <- as.data.frame(i_table_under_15)
      
      i_table_under_15 <- i_table_under_15 %>%
        filter(i_water_under_15 != 0) %>%
        rename(
          `Community_Type` = hh6,
          `Female_Under_15` = Freq
        )
      #names(i_table_under_15)[names(i_table_under_15) == "i_water_under_15"] <- "country"
      i_table_under_15$country <- i
      i_table_under_15 <- i_table_under_15 %>%
        select(-i_water_under_15)
      i_table_under_15$`Community_Type` <- ifelse(i_table_under_15$`Community_Type` == 1, 'Urban', 'Rural')
    }
    
    i_weighted_freqs_over_15 <- svytable(~i_water_over_15 + hh6, mysurvey)
    i_weighted_freqs_all_ages <- svytable(~i_water_all_ages + hh6, mysurvey)
    
    i_proportions_over_15 <- prop.table(i_weighted_freqs_over_15, margin = 2)
    i_proportions_all_ages <- prop.table(i_weighted_freqs_all_ages, margin = 2)
    
    # Store proportions in i_table_over_15
    i_table_over_15 <- i_proportions_over_15
    i_table_all_ages <- i_proportions_all_ages
    
    # Filter out rows with 0
    i_table_all_ages <- as.data.frame(i_table_all_ages)
    i_table_over_15 <- as.data.frame(i_table_over_15)
    
    i_table_all_ages <- i_table_all_ages %>%
      filter(i_water_all_ages != 0) %>%
      rename(
        country = i_water_all_ages,
        `Community_Type` = hh6,
        `Female_Any_Age` = Freq
      )
    
    i_table_over_15 <- i_table_over_15 %>%
      filter(i_water_over_15 != 0) %>%
      rename(
        country = i_water_over_15,
        `Community_Type` = hh6,
        `Female_Over_15` = Freq
      )
    
    # Assign country codes
    i_table_all_ages$country <- i
    i_table_over_15$country <- i
    
    # Assign Community_Type based on values
    i_table_all_ages$`Community_Type` <- ifelse(i_table_all_ages$`Community_Type` == 1, 'Urban', 'Rural')
    i_table_over_15$`Community_Type` <- ifelse(i_table_over_15$`Community_Type` == 1, 'Urban', 'Rural')
    
    # Combine tables
    output_all_under <- left_join(i_table_all_ages, i_table_under_15, by = c("country", "Community_Type"))
    output <- left_join(output_all_under, i_table_over_15, by = c("country", "Community_Type"))
    
    # Append to df
    MICS3_df1 <- rbind(MICS3_df1, output)
    
    } 
    
    else {
    # If either condition is not met, handle it here
    # Create a data frame with NA values for the specific structure
    i_table_na <- data.frame(
      country = rep(i, 2),
      Community_Type = c("Urban", "Rural"),
      Female_Any_Age = rep(NA, 2),
      Female_Under_15 = rep(NA, 2),
      Female_Over_15 = rep(NA, 2)
    )
    
    # Append to df
    MICS3_df <- rbind(MICS3_df1, i_table_na)
    }
  } 
}


MICS3_UR <- left_join(MICS3_df1, MICS3_df2, by = c("country", "Community_Type"))
#write_xlsx(df, path = "MICS_3.xlsx") double check will not make new csv