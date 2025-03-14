#taken from search_results_v2_yc.xlsx to read(TREAT YES)
library(readxl)
read_TREAT_YES <- read_excel("~/Downloads/Olivia_read_TREAT_YES.xlsx")
names(read_TREAT_YES)[2] = "name"
files_want <- as.vector(read_TREAT_YES['name'])
class(files_want) 


files_all = paste0(box_path, "Safe water womens time full text copy")

files_all2 <- data.frame(list.files(files_all, all.files = TRUE))


## Find all files matching the pattern
test_files <- list.files(
  path = tempdir(), # replace with the directory you want
  pattern = "test.*\\.csv$", # has "test", followed by 0 or more characters,
  # then ".csv", and then nothing else ($)
  full.names = TRUE # include the directory in the result
)

foldernames<-sub("^([^.]*).*", "\\1", list.files(files_all, all.files = files_want)) 
foldernames<-paste("out_put/",foldernames,sep='')
lapply(foldernames,dir.create,recursive = TRUE)

files_filter(files_all, filter.by, full.names = "Abubakar 2021 Understanding")