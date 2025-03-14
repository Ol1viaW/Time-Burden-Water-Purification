
#Guinea-Bissau not on this list???
#Argentina, Dominican Republic no area so just do total
#Costa Rica, Serbia, Serbia_Roma Settlements, Republic_of_North_Macedonia, Republic_of_North_Macedonia_Roma_Settlements skip water treatment questions so currently dropping
#Dominican Republic & Honduras & Belarus skip solar disinfection water treatment question so put NA
#Belarus has no solar disinfection column so put NA
#Sao_Tome_and_Principe use HL4 column?
#Nigeria NA values in weights

countryIds2 = c("Afghanistan", "Algeria", "Argentina", "Bangladesh", "Belarus", "Benin", "Central_African_Republic", "Chad", "Comoros",
               "Congo", "Costa Rica", "Cuba", "Dominican Republic", "Eswatini", "Fiji", "Gambia", "Georgia", "Honduras",
               "Iraq", "Jamaica", "Kiribati", "Kosovo_under_UNSC_res_1244", "Kosovo_UNSCR_1244_Roma_Ashkali_Egyptian Communities", "Lao", "Lesotho", "Madagascar", "Malawi", "Mongolia", 
               "Montenegro","Montenegro_Roma_Settlements", "Nepal", "Nigeria", "Pakistan_Balochistan",
               "Pakistan_Khyber_Pakhtunkhwa", "Pakistan_Punjab", "Pakistan_Sindh", "Republic_of_North_Macedonia","Republic_of_North_Macedonia_Roma_Settlements",
               "Samoa", "Sao_Tome_and_Principe", "Serbia", "Serbia_Roma Settlements", "Sierra_Leone", "Suriname", "Thailand", "Tonga", "Tunisia",
               "Turkmenistan", "Tuvalu", "Uzbekistan", "Vanuatu", "Viet_Nam", "Yemen", "Zimbabwe") 

countryIds = c("Afghanistan", "Algeria", "Bangladesh", "Benin", "Central_African_Republic", "Chad", "Comoros",
               "Congo", "Cuba", "Dominican Republic", "Eswatini", "Fiji", "Gambia", "Georgia", "Honduras",
               "Iraq", "Jamaica", "Kiribati", "Kosovo_under_UNSC_res_1244", "Kosovo_UNSCR_1244_Roma_Ashkali_Egyptian Communities", "Lao", "Lesotho", "Madagascar", "Malawi", "Mongolia", 
               "Montenegro","Montenegro_Roma_Settlements", "Nepal", "Pakistan_Balochistan",
               "Pakistan_Khyber_Pakhtunkhwa", "Pakistan_Punjab", "Pakistan_Sindh",
               "Samoa", "Sierra_Leone", "Suriname", "Thailand", "Tonga", "Tunisia",
               "Turkmenistan", "Tuvalu", "Uzbekistan", "Vanuatu", "Viet_Nam", "Yemen", "Zimbabwe") 

#currently dropping countries here that does not include all the questions we want

#Dominican Republic & Honduras skip solar disinfection water treatment question
#Costa Rica, Serbia, Serbia_Roma Settlements, Republic_of_North_Macedonia, Republic_of_North_Macedonia_Roma_Settlements skip water treatment questions so currently dropping
#Mongolia Drink water without making any treatment
#can't find Argentina, Dominican Republic area
#Belarus has no solar disinfection column
#Sao_Tome_and_Principe can't find sex drop?
#Nigeria hhweightMICS called Household sample weight (MICS) vs hhweight called Household sample weight (MICS-NICS)

MICS6_df1 = data.frame()
MICS6_df2 = data.frame()
# Loop through each country 
for (i in countryIds) {
  file_hl <- paste0(mics6_dta, "/", i, "/", "hl", ".sav")
  file_hh <- paste0(mics6_dta, "/", i, "/", "hh", ".sav")
  i_hl <- read_sav(file_hl)
  i_hh <- read_sav(file_hh)
  colnames(i_hl) <- trimws(colnames(i_hl))
  colnames(i_hh) <- trimws(colnames(i_hh))
  
  MICS6_hh_labels <- c(
    "Cluster", #hh1
    "Household number", #hh2
    "Area", #hh6
    "Main source of drinking water", #WS1
    "Person fetching water", #WS5
    "Treat water to make water safer to drink", #WS9
    "Water treatment: Boil", #WS10A
    "Water treatment: Add bleach/chlorine", #WS10B
    "Water treatment: Strain it through a cloth", #WS10C
    "Water treatment: Use water filter", #WS10D
    "Water treatment: Let it stand settle", #WS10F
    "Household weight",
    "Water treatment: Solar disinfection" #WS10E
  ) 
  #12 columns
  #var_label(i_hh$WS1)
  
  MICS6_water_labels_hl <- c(
    "Cluster", #hh1
    "Household number", #hh2
    "Line number", #hl1
    "Sex", #hl4
    "Age", #hl5
    "Household weight" #hhweight
  )
  #6 columns
  
  select_by_labels <- function(dataframe, labels) {
  column_names <- vector("character", length(labels))
  
  for (i in seq_along(labels)) {
    label <- labels[i]
    matched_col <- names(dataframe)[sapply(dataframe, function(col) {
      lbl <- var_label(col)
      if (is.null(lbl)) return(FALSE)
      lbl <- unlist(lbl)
      return(label %in% lbl)
    })]
    
    if (length(matched_col) > 0) {
      matched_col <- matched_col[1]
      column_names[i] <- matched_col
    }
    
    if (length(matched_col) == 0 && label == "Cluster") { #hh1
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Cluster number", "Numero de la grappe", "Numéro séquenetiel de la grappe", "Numéro de Grappe","Numéro de grappe", "Numéro de la grappe", "Número de conglomerado", "Número de Conglomerado(UPM)", "Número de conglomerado o UPM", "Número do DR Nouvel ordre",
                              "Numéro de área de Enumeração", "Código de consultorio", "Número de área de Enumeração")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Household number") { #hh2
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Numero du menage", "Numéro du ménage", "Número de hogar", "Número de Hogar(Vivienda)", "Número de orden de la vivienda en la muestra", 
                              "Número do Agregado Familiar", "Numéro de AF", "Numéro de ménage", "Número de AF")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Line number") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Número de línea", "Número de linha", "Número de ligne", "Numéro de ligne",
                              "Numéro de la ligne", "Numéro de la linha", "Numéro de linha", "Número da linha")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Age") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("¿Cuántos años cumplidos tiene (nombre)?", "Idade", "Edad")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Sex") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Sexe", "Sexo")))
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
        return(any(lbl %in% c("Urban_Rural", "Milieu", "Milieu de résidence", "Milieu de Résidence", "Área", "Area of residence",
                              "Meio de residência",  "Zona de residencia")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Primary sampling unit") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("PSU", "Unidad primaria de muestreo", "Unités primaires d'échantillonnage", "Número de conglomerado", "Cluster number")))
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
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Household sample weight", "Poids de ponderation menage", "Poids échantillon ménages", "Household Weight", "Weights", "Ponderación del hogar",
                              "Ponderadores para agregados familiares", "Ponderador de amostra agregados familiares", "Poids d'échantillonnage du ménage", "Poids de l'échantillon du ménage",
                              "Poids de l'échantillon des ménages", "Peso de muestra del hogar", "Ponderador de hogares", "Factor ponderación del hogar", "Pesos muestral del hogar",
                              "Ponderador de amostra agregados familiares", "Household sample weight (MICS)")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Time to get water and come back") {
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("time to get water and come back", "Temps pour aller chercher de leau et revenir")))
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
                              "Source principale d'eau de boisson", "Principal fuente de agua para beber", "De onde provém a água utilizada pelos membros do agregado principalmente para beber",
                              "De onde provém a água utilizada pelos membros do agregado principalmente para beber?", "Quelle est la source principale d’eau de boisson utilisée par les membres de votre ménage",
                              "Quelle est la source principale d’eau de boisson utilisée par les membres de votre ménage", "¿Cuál es la fuente principal de agua para beber que utilizan los miembros de este hogar?")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Person fetching water") { #WS5
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl)
        return(any(lbl %in% c("Person collecting water", "Qui se rend habituellement a la source d'eau", "Personne habituellement chargée de chercher de l'eau", "Qui va chercher principalement de l'eau", "Qui va chercher de l'eau", "Persona quien recolecta agua", 
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
    
    if (length(matched_col) == 0 && label == "Treat water to make water safer to drink") { #WS9
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Treats water", "Treat water to make safer for drinking", "Faites-vous, ou n'importe quel autre membre de ce ménage, quelque chose à l'eau pour la rendre plus saine à boire",
                              "Eau traitée pour la rendre saine à boire", "Traiter l'eau pour la rendre plus saine à boire", "Traite l'eau avant de la boire", "El agua fue tratada para que sea más segura para beber",
                              "El agua fue tratada para que sea segura para beber", "Você ou outra pessoa do AF faz alguma coisa para tornar a água potável para beber?",
                              "Faites-vous, ou n’importe quel autre membre de ce ménage fait-il quelque chose à l’eau pour la rendre plus saine à boire", "Quelque chose est faite à l'eau pour la rendre plus saine à boire",
                              "¿Usted o algún miembro de este hogar le dan algún tratamiento al agua o le hace algo para hacerla más segura para beber?",
                              "Done anything to the water to make it safer to drink", "Drink water without making any treatment")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Boil") { #WS10A
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Boil", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: LA FAIRE BOUILLLIR",
                              "Traitement de l'eau: Bouillir", "Traitement de l'eau : Bouillir", "Tratamiento de agua: hervir", "Tratamiento de agua: hervir",
                              "Water treatment: boil", "Bouillir", "Tratamiento de agua: LA HIERVEN","Traitement de l'eau: BOUILLIR", 
                              "A. Ferve-a")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Add bleach/chlorine") { #WS10B
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Add bleach/chlorine", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:AJOUTER DE LA JAVEL/ CHLORE",
                              "Traitement de l'eau: Ajout de javel/chlore", "Traitement de l'eau : Ajout de javel/chlore", "Traitement de l'eau : Ajouter de la javel/chlore",
                              "Traitement de l'eau: Ajouter de la javel/ chlore", "Tratamiento de agua: agrega hiclorito / chloro", "Water treatment: Add jikbleach/chlorine",
                              "Tratamiento de agua: agregue blanqueador / cloro", "Water treatment: Add bleach / chlorine", "Water treatment: add bleach/chlorine",
                              "Water treatment: Add bleach/Chlorine", "B. Adiciona lixivia ou Pastilha de cloro", "Ajouter dela javel/ chlore", "Traitement de l'eau: Ajouter de la Javel/Chlore",
                              "Tratamiento de agua: agrega hiclorito / cloro", "Tratamiento de agua: LE AÑADEN CLORO", "Traitement de l'eau: AJOUTER DE LA JAVEL / CHLORE")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Strain it through a cloth") { #WS10C
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: strain it through a cloth", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:FILTRER A TRAVERS UN TISSU",
                              "Traitement de l'eau: Filtrer avec un tissu", "Traitement de l'eau: Filtrer avec un tissu", "Traitement de l'eau : Filtrer avec un tissu", "Traitement de l'eau : Filtrer a travers un tissu",
                              "Traitement de l'eau: Filtrer a travers un tissu", "Tratamiento de agua: filtra con una tela", "Tratamiento de agua: filtra con una tela", "Tratamiento de agua: filtra con una tela",
                              "Water treatment: strain it through a cloth", "C. Filtra com pano", "Filtrer a travers un tissu", "Traitement de l'eau: Filtrer à travers un tissu", "Tratamiento de agua:  filtra con una tela",
                              "Tratamiento de agua: LA FILTRAN O LA CUELAN CON UNA TELA", "Traitement de l'eau: FILTRER A TRAVERS UN TISSU", "Water treatment: Use filter cloth", "Traitement de l'eau : Filtrer à travers un tissu"
                              )))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Use water filter") { #WS10D
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Use water filter", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:UTILISER UN FILTRE A EAU (CERAMIQUE, SABLE, COMPOSITE, ETC.)",
                              "Traitement de l'eau: Utiliser un filtre à eau", "Traitement de l'eau : Utiliser un filtre à eau", "Tratamiento de agua: filtro de agua", "Water treatment: use water filter",
                              "D. Usa Filtro (cerâmica), areia e compositos", "Traitement de l'eau: Utiliser un filtre a eau (ceramique, sable, composite, etc.)", "Utiliser un filtre a eau (ceramique, sable, composite, etc.)",
                              "Traitement de l'eau: Utiliser un filtre à eau (céramique, sable, composite)", "Tratamiento de agua: filtro de agua (cerámica, arena, compuestos)",
                              "Tratamiento de agua: UTILIZAN UN FILTRO PARA AGUA (CERÁMICA, ARENA, COMPUESTOS, ETC.)", "Water treatment: Use water filter (ceramic, sand, composite, etc.)",
                              "Traitement de l'eau: UTILISER UN FILTRE A EAU (CERAMIQUE, SABLE, COMPOSITE, ETC.)")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Solar disinfection") { #WS10E
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Solar disinfection", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: DESINFECTION SOLAIRE",
                              "Traitement de l'eau: Désinfection solaire", "Traitement de l'eau : Désinfection solaire", "Traitement de l'eau : Desinfection solaire",
                              "Tratamiento de agua: Desinfección solar", "Water treatment: Sunlight Exposure (Solar disinfection)", "Water treatment: solar disinfection",
                              "E. Desinfecção Solar", "Traitement de l'eau: Desinfection solaire",  "Desinfection solaire")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Let it stand settle") { #WS10F
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Let it stand and settle", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: LA LAISSER REPOSER ET DECANTER", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: LA LAISSER REPOSER ET DECANTER",
                              "Traitement de l'eau: Laisser reposer et décanter", "Traitement de l'eau : Laisser reposer et décanter", "Traitement de l'eau : Laisser réposer et décanter", "Traitement de l'eau : La laisser reposer et decanto", "Tratamiento de agua: dejarla reposar y asentarse", 
                              "Tratamiento de agua: dejarlo reposar y asentarse", "Water treatment: Let it stand and settle", "Water treatment: let it stand and settle", "F. Deixa assentar e decantar",
                              "Traitement de l'eau: La laisser reposer et decanter", "La laisser reposer et decanter", "Traitement de l'eau: La laisser reposer et décanter", "Tratamiento del agua: dejarla reposar y asentarse",
                              "Tratamiento del agua: LA DEJAN REPOSAR Y ASENTAR", "Tratamiento del agua: dejarlo reposar y asentarse")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: Other") { #WS10X
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: Other", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: AUTRE (Préciser)",
                              "Traitement de l'eau: Autre", "Traitement de l'eau : Autre", "Tratamiento de agua: otro", "Water treatment: other", "X. Outra", "Autre",
                              "Traitement de l'eau: Autres", "Tratamiento de agua: OTRO (Especifique)")))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: DK") { #WS10Z
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: DK", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire:NSP",
                              "Traitement de l'eau: NSP", "Traitement de l'eau : NSP", "Tratamiento de agua: No sabe", "Z. Não sabe", "NSP", "Tratamiento de agua: No sabe" )))
      })]
      if (length(matched_col) > 0) {
        matched_col <- matched_col[1]
        column_names[i] <- matched_col
      }
    }
    
    if (length(matched_col) == 0 && label == "Water treatment: No response") { #WS10NR
      matched_col <- names(dataframe)[sapply(dataframe, function(col) {
        lbl <- var_label(col)
        if (is.null(lbl)) return(FALSE)
        lbl <- unlist(lbl) #making sure format matches
        return(any(lbl %in% c("Water Treatment: No response", "Que faites-vous habituellement à l'eau pour la rendre plus saine à boire: NON REPONSE",
                              "Traitement de l'eau: Manquant", "Traitement de l'eau : Manquant", "Traitement de l'eau : Non response", "Tratamiento de agua: Ignorado",  "Water treatment: Missing",
                              "Water treatment: no response", "Não responde", "Traitement de l'eau: Non reponse", "Non reponse")))
      })]
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
  i_hl_w <- select_by_labels(i_hl, MICS6_water_labels_hl)
  i_hh_w <- select_by_labels(i_hh, MICS6_hh_labels)

  if (i == "Dominican Republic" |i == "Belarus" | i == "Honduras") {
    solar_disinfection_col <- which(names(i_hh_w) == "Water treatment: Solar disinfection")
    if (length(solar_disinfection_col) > 0) {
      i_hh_w[, solar_disinfection_col] <- NA
    } else {
      i_hh_w$`Water treatment: Solar disinfection` <- NA
    }
  }
  
  new_column_names_hl <- c("hh1", "hh2", "hl1", "hl4", "hl5", "hhweight")
  new_column_names_hh <- c("hh1", "hh2", "hh6", "WS1", "WS5", "WS9", "WS10A", "WS10B", "WS10C", "WS10D", "WS10F", "hhweight", "WS10E")
  names(i_hl_w) <- new_column_names_hl
  names(i_hh_w) <- new_column_names_hh

  if (ncol(i_hl_w) > 5 && ncol(i_hh_w) > 12) {
    
    ##------
    #Code for Water treatment questions 
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
        table_var <- as.data.frame(proportions) 
        if (nrow(table_var) > 0) {
          table_var <- table_var %>%
            filter(!!sym(var) != 0) %>%
            rename(
              country = !!sym(var),
              Community_Type = hh6
            ) %>%
            rename_with(~ if_else(. == "Freq", var, .))
        }
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
    }
    MICS6_df2 <- rbind(MICS6_df2, combined_results)
    ##------
    #End code for non-water fetching questions 
    ##-----
      
  } else {
    #don't want using combined_df from previous country
    combined_df_w <- data.frame()
    print('missing column(s)')
    print(i)
  }

  combined_df_w <- left_join(i_hh_w, i_hl_w, 
                             by = c("hh1" = "hh1", "hh2" = "hh2", "WS5" = "hl1", "hhweight" = "hhweight"))
  # Check if WS5 column has any non-NA values
  if (!all(is.na(combined_df_w$WS5))) {

    # If both conditions are true, execute this block
    
    # Drop roWS where WS5 is NA
    #combined_dfa <- na.omit(combined_df)
    
    if (i == "Lao") {
      combined_df_w$HH6 <- ifelse(combined_df_w$hh6 == 1, 1, 
                                  ifelse(combined_df_w$hh6 == 2 | combined_df_w$hh6 == 3, 2, NA_real_))
      
      combined_df_w <- combined_df_w %>%
        rename(
          hh6 = HH6,      # Rename HH6 to hh6
          old_hh6 = hh6   # Rename old hh6 to old_hh6
        ) }
      #if (i == "Lesotho") {
      #  combined_df_w$HH6 <- ifelse(combined_df_w$hh6 == 3, 2, 
       #                             ifelse(combined_df_w$hh6 == 1 | combined_df_w$hh6 == 2, 1, NA_real_))
        
        #combined_df_w <- combined_df_w %>%
         # rename(
          #  hh6 = HH6,      # Rename HH6 to hh6
           # old_hh6 = hh6   # Rename old hh6 to old_hh6
          #)
    #}
    
    # Create new columns based on conditions and handle NA values
    combined_df_water <- combined_df_w %>%
      mutate(
        i_water_all_ages = ifelse(is.na(hl4), NA, ifelse(hl4 == 2, 1, 0)),
        i_water_under_15 = ifelse(is.na(hl4) | is.na(hl5), NA, ifelse(hl4 == 2 & hl5 <= 15, 1, 0)),
        i_water_over_15 = ifelse(is.na(hl4) | is.na(hl5), NA, ifelse(hl4 == 2 & hl5 > 15, 1, 0))
      )
    
    # Weighting using household sample weight
    dta <- combined_df_water
    dta$wt <- dta$hhweight
    
    # Create survey design object
    mysurvey <- svydesign(id = ~hh1, data = dta, strata = ~hh6,
                            weight = ~wt, nest = TRUE)
    
    # Adjust options for survey package
    options(survey.lonely.psu = "adjust")
    
    i_weighted_freqs_over_15 <- svytable(~i_water_over_15 + hh6, mysurvey)
    i_weighted_freqs_under_15 <- svytable(~i_water_under_15 + hh6, mysurvey)
    i_weighted_freqs_all_ages <- svytable(~i_water_all_ages + hh6, mysurvey)
    
    i_proportion_over_15 <- prop.table(i_weighted_freqs_over_15, margin = 2)
    i_proportion_under_15 <- prop.table(i_weighted_freqs_under_15, margin = 2)
    i_proportion_all_ages <- prop.table(i_weighted_freqs_all_ages, margin = 2)
    
    i_proportions_over_15 <- i_proportion_over_15 * 100 #want to turn into percent
    i_proportions_under_15 <- i_proportion_under_15 * 100 #want to turn into percent
    i_proportions_all_ages <- i_proportion_all_ages * 100 #want to turn into percent
    
    # Store proportions in i_table_over_15
    i_table_over_15 <- i_proportions_over_15
    i_table_under_15 <- i_proportions_under_15
    i_table_all_ages <- i_proportions_all_ages
    
    i_table_all_ages <- as.data.frame(i_table_all_ages)
    
    i_table_all_ages <- i_table_all_ages %>%
      filter(i_water_all_ages != 0) %>%
      rename(
        country = i_water_all_ages,
        `Community_Type` = hh6,
        `Female_Any_Age` = Freq
      )
    # Assign country codes
    i_table_all_ages$country <- i
    
    if (nrow(i_table_all_ages) < 3) {
      i_table_all_ages <- i_table_all_ages %>%
        mutate(
          Community_Type = case_when(
            Community_Type == 1 ~ 'Urban',
            TRUE ~ 'Rural'
          )
        )
    } else if (nrow(i_table_all_ages) == 3) {
      i_table_all_ages <- i_table_all_ages %>%
        mutate(
          Community_Type = case_when(
            Community_Type == 1 ~ 'Urban',
            Community_Type == 2 ~ 'Rural',
            Community_Type == 3 ~ 'Other'
          )
        )
    }
    
    i_table_over_15 <- as.data.frame(i_table_over_15)
    
    i_table_over_15 <- i_table_over_15 %>%
      filter(i_water_over_15 != 0) %>%
      rename(
        country = i_water_over_15,
        `Community_Type` = hh6,
        `Female_Over_15` = Freq
      )
    
    i_table_over_15$country <- i
    
    if (nrow(i_table_over_15) < 3) {
      i_table_over_15 <- i_table_over_15 %>%
        mutate(
          Community_Type = case_when(
            Community_Type == 1 ~ 'Urban',
            TRUE ~ 'Rural'
          )
        )
    } else if (nrow(i_table_over_15) == 3) {
      i_table_over_15 <- i_table_over_15 %>%
        mutate(
          Community_Type = case_when(
            Community_Type == 1 ~ 'Urban',
            Community_Type == 2 ~ 'Rural',
            Community_Type == 3 ~ 'Other'
          )
        )
    }
    
    i_table_under_15 <- as.data.frame(i_table_under_15)
    num_rows <- nrow(i_table_under_15)
    if (num_rows > 2) {
      i_table_under_15 <- i_table_under_15 %>%
        filter(i_water_under_15 != 0) %>%
        rename(
          country = i_water_under_15,
          Community_Type = hh6,
          Female_Under_15 = Freq
        )
      
      i_table_under_15$country <- i
      
      if (nrow(i_table_under_15) < 3) {
        i_table_under_15 <- i_table_under_15 %>%
          mutate(
            Community_Type = case_when(
              Community_Type == 1 ~ 'Urban',
              TRUE ~ 'Rural'
            )
          )
      } else if (nrow(i_table_under_15) == 3) {
        i_table_under_15 <- i_table_under_15 %>%
          mutate(
            Community_Type = case_when(
              Community_Type == 1 ~ 'Urban',
              Community_Type == 2 ~ 'Rural',
              Community_Type == 3 ~ 'Other'
            )
          )
      }
    } else {
      # Create a table with 2 rows, first column 'country' with value 'i' and 'Female_Under_15' with NA
      i_table_under_15 <- data.frame(
        country = rep(i, 2),
        Community_Type = c("Urban", "Rural"),
        Female_Under_15 = NA
      )
    }
    output_all_under <- left_join(i_table_all_ages, i_table_under_15, by = c("country", "Community_Type"))
    output <- left_join(output_all_under, i_table_over_15, by = c("country", "Community_Type"))
    
    # Append to df
    MICS6_df1 <- rbind(MICS6_df1, output)
    
    
  } else {
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
    MICS6_df1 <- rbind(MICS6_df1, i_table_na)
  }
} 

MICS6_UR <- left_join(MICS6_df1, MICS6_df2, by = c("country", "Community_Type"))
