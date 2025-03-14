#note must run the Olivia_Practice_rdsh_download.R and rdhs_group_region.R files

CLASS <- read_excel("~/Downloads/CLASS.xlsx")
View(CLASS)
names(CLASS)[1] <- "country"

# Merge UNSD country list with the DHS API data, "dta" 
dim(group_region)
dim(CLASS)
group_wealth<-left_join(group_region, CLASS, by = "country")
dim(group_wealth)
#length(unique(all_indicators$surveyid)) #number of unique surveys
length(unique(group_wealth$country)) #number of unique countries should be 36

drop <- c("Code", "Lending category")
group_wealth = group_wealth[ , !(names(group_wealth) %in% drop)]

write.csv(group_wealth, paste0(here::here(), "/1-dm/countryindicatorsbywealthworldwide.csv"))