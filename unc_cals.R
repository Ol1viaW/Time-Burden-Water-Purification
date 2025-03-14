# all MICS and DHS counntries so far

dt = read.csv("1-dm/DHS_MICS_Oct_6.csv") 
pop = read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_31753.csv", skip=4, header=TRUE); head(pop)

dtt = dt %>% filter(Community.Type == "Total")
pop = pop %>% select(c(Country.Name, Indicator.Name, X2023))

df = left_join(dtt, pop, by=c("country" = "Country.Name"))
nrow(df)

View(df)

tx = c("Boil", "Bleach", "Filter", "SolarDisinfection")
df = df %>% 
  select(country, Type, SurveyYear, X2023, Boil, Bleach, Filter, SolarDisinfection) %>%
  mutate(across(tx, round, 1))

# No data from: Honduras, Dominican Republic, Nigeria, Sao Tome and Principe

df$X2023[df$country == "Congo"] <- 102262808
missing = c("Honduras", "Dominican Republic", "Nigeria", "Sao_Tome_and_Principe", "Mongolia")
df = df %>% filter(!(country %in% missing))

# Survey year and population for Central African Republic
df$X2023[df$country == "Central_African_Republic"] <- 5742315
df$SurveyYear[df$country=="Central_African_Republic"] <- 2019
  
# Drop Egypt MICS and add population
df = df %>% filter(!(country=="Egypt" & Type=="MICS"))
df$X2023[df$country == "Egypt"] <- 112716598
  
# Add population Gambia and drop MICS
df = df %>% filter(!(country=="Gambia" & Type=="MICS"))
df$X2023[df$country == "Gambia"] <- 2773168
  
# drop other Indonesia districts for MICS
df = df %>% filter(!(country=="Indonesia_Papua_Selected_Districts" | country=="Indonesia_West_Papua_Selected_Districts"))

# Check Kosovo population
df = df %>% filter(!(country=="Kosovo_UNSCR_1244_Roma_Ashkali_Egyptian Communities"))
df$country[df$country=="Kosovo_under_UNSC_res_1244"] <- "Kosovo"
df$X2023[df$country == "Kosovo"] <- 1756374

# Add Lao population
df$X2023[df$country == "Lao"] <- 7633779
  
# Drop Montenegro Roma Settlements
df = df %>% filter(!(country=="Montenegro_Roma_Settlements"))

# Drop other Pakistan groups
p_drop = c("Pakistan_Balochistan", "Pakistan_Gilgit_Baltistan", "Pakistan_Khyber_Pakhtunkhwa", 
           "Pakistan_Punjab", "Pakistan_Sindh")
df = df %>% filter(!(country %in% p_drop))

# Drop MICS Sierra Leone
df = df %>% filter(!(country=="Sierra_Leone" & Type=="MICS"))

# Drop Somalia
df = df %>% filter(!(country=="Somalia_Northeast_Zone" | country=="Somalia_Somaliland"))

# Population for South Sudan
df$X2023[df$country == "South_Sudan"] <- 11088796
  
# Add population Vietnam
df$X2023[df$country == "Viet_Nam"] <- 98858950
df$country[df$country=="Viet_Nam"] <- "Vietnam"

# Add population Yemen
df$X2023[df$country == "Yemen"] <- 34449825

# calculate boiling hours
df = df %>%
  mutate(boil_dailyhrs= (X2023/5)*(Boil/100)*(20/60))

df = df %>% mutate(boil_dailyhrs1000s = round(boil_dailyhrs/1000, 0))

# export formatted table
write.csv(df, "unc_post_table.csv")
