#----------------------
# parse practice texts for key words
#----------------------

# Setting up file paths
# drivepath<-setwd("/Users/cdelmera/Library/CloudStorage/GoogleDrive-cdelmera@stanford.edu/.shortcut-targets-by-id/1_6wp0kjH__gi4qUHXp-mq3Z4PGGCJR0U/Women time use safe water/safe-water-time/Full text pdfs")
# fulltext<-paste0(drivepath,"/safe-water-time/Full text pdfs")
practicetexts<- list.files(practicetexts, pattern = "pdf$")
practicetexts <- system.file('pdf', package = 'pdfsearch')

# unlisting and converting to lower case letters
practicetexts <- unlist(practicetexts)
practicetexts <- tolower(practicetexts)


# Scanning relevant papers + include abbreviations and french translations as keywords
Practice_Relevant_papers<-keyword_directory(practicetexts, keyword = c(" time ","minutes","hours","seconds"," tiempo ","minutos","horas","segundos", " temps ", "heures", "secondes", " min ", " hr ", " sec ", " h ", " seg ", " sec"), surround_lines = 1)

#Deals with Error in x[is.na(x)] <- na.string : replacement has length zero when exporting data frame to openxlsx 
#Error was caused by cells containing vectors. This line of code uses across to modify the vector to character.

Practice_Relevant_papers <- Practice_Relevant_papers %>% mutate(across(where(is.list), as.character))
openxlsx::write.xlsx(Practice_Relevant_papers, practicetexts)


write.xlsx(
  Practice_Relevant_papers,
  file = "Practice_search_results.xlsx",
  sheetName = "results",
  colNames = TRUE,
  rowNames = TRUE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)

View(Practice_Relevant_papers)

#----------------------
# export as xlsx
#----------------------

