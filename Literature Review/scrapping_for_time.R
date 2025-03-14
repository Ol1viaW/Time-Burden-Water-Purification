#----------------------
# parse full texts for key words
#----------------------

# Setting up file paths
# drivepath<-setwd("/Users/cdelmera/Library/CloudStorage/GoogleDrive-cdelmera@stanford.edu/.shortcut-targets-by-id/1_6wp0kjH__gi4qUHXp-mq3Z4PGGCJR0U/Women time use safe water/safe-water-time/Full text pdfs")
# fulltext<-paste0(drivepath,"/safe-water-time/Full text pdfs")
fulltext <- list.files(fulltexts, pattern = "pdf$")
fulltext <- system.file('pdf', package = 'pdfsearch')

# unlisting and converting to lower case letters
fulltext<-unlist(fulltexts)
fulltext<-tolower(fulltext)


# Scanning relevant papers + include abbreviations and french translations as keywords
Relevant_papers<-keyword_directory(fulltext, keyword = c(" time "," minutes "," hours "," seconds "," tiempo "," minutos "," horas "," segundos ", " temps ", " heures ", " secondes ", " min ", " hr ", " sec ", " h ", " seg "), surround_lines = 1)

#Deals with Error in x[is.na(x)] <- na.string : replacement has length zero when exporting data frame to openxlsx 
#Error was caused by cells containing vectors. This line of code uses across to modify the vector to character.

Relevant_papers <- Relevant_papers %>% mutate(across(where(is.list), as.character))
openxlsx::write.xlsx(Relevant_papers, fulltext)


write.xlsx(
  Relevant_papers,
  file = "search_results.xlsx",
  sheetName = "results",
  colNames = TRUE,
  rowNames = TRUE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)

View(Relevant_papers)

#----------------------
# export as xlsx
#----------------------
