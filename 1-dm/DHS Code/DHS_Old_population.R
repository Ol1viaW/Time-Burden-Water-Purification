population <- read_excel("~/Library/CloudStorage/Box-Box/safewatertime/data/P_Data_Extract_From_Population_estimates_and_projections.xlsx")
population <- population[ -c(2,4) ]
names(population)[2] <- "CommunityType"
names(population)[1] <- "Country"
population$Country[population$Country=="Egypt, Arab Rep."] <- "Egypt"

# Split name column into firstname and last name
population <- population %>% separate(CommunityType, c('CommunityType', 'drop'))
population$CommunityType<-gsub("population","",as.character(population$CommunityType))
population <- population[-(3)]
population_urban_rural <- population
population_total <- population
population_urban_rural$CommunityType <- gsub("Population","",as.character(population_urban_rural$CommunityType))
population_urban_rural <- population_urban_rural[!(population_urban_rural$CommunityType == ""), ]

population_total$CommunityType <- gsub("Rural","",as.character(population_total$CommunityType))
population_total <- population_total[!(population_total$CommunityType == ""), ]
population_total$CommunityType <- gsub("Urban","",as.character(population_total$CommunityType))
population_total <- population_total[!(population_total$CommunityType == ""), ]
colnames(population_total) <- gsub(" \\[YR.*\\]", "", colnames(population_total))
colnames(population_urban_rural) <- gsub(" \\[YR.*\\]", "", colnames(population_urban_rural))

names(population_urban_rural)[2] <- "Community Type"
names(population_total)[2] <- "Community Type"

boil <- dhs_data(indicatorIds = c("WS_WTRT_H_BOL"), countryIds, surveyYearStart = 2000, selectSurvey="latest", breakdown = "background") %>%
  filter(CharacteristicLabel == "Urban" | CharacteristicLabel == "Rural") %>%
  select(CountryName, Value, CharacteristicLabel, SurveyYear) %>% #select columns of interest
  relocate(CountryName,CharacteristicLabel)
names(boil)[1] <- "Country" 
names(boil)[2] <- "Community Type" #label for Rural/Urban
names(boil)[3] <- "Boil" #rename value column, which is the percentage of population that boils water
#able to pull data from all selected countries

boil_population <- merge(boil, population_urban_rural, by = c("Country", "Community Type"), all = TRUE)
names(boil_population)[5] <- "2006"
names(boil_population)[6] <- "2008"
names(boil_population)[7] <- "2012"
names(boil_population)[8] <- "2013"
names(boil_population)[9] <- "2014"
names(boil_population)[10] <- "2015"
names(boil_population)[11] <- "2016"
names(boil_population)[12] <- "2017"
names(boil_population)[13] <- "2018"
names(boil_population)[14] <- "2019"
names(boil_population)[15] <- "2020"
names(boil_population)[16] <- "2021"
names(boil_population)[17] <- "2022"

boil_population <- boil_population[order(boil_population$SurveyYear, decreasing = TRUE),]
boil <- boil[order(boil$SurveyYear, decreasing = TRUE),]

boil_population <- boil_population %>%
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

names(boil_population)[2] = "Community"
boil_population <- boil_population %>%
  select(Country, Community, Boil, population_2) %>%
  drop_na(Boil)
boil_population <- boil_population[order(boil_population$Country, decreasing = FALSE),]
names(boil_population)[4] = "Community Population"
names(boil_population)[2] = "Community Type"
