## Objective: To identify SU directories containing 'Data' subdirectories 
### and to extract the files from 'Data' to the main SU Directory

library(tidyverse)

### Enter information about the SU of interest
Year <- 2023
State <- 'Washington'
SU <- 114895

### List the directories in the Raw data directory
setwd(paste("E:","NABat",State,"Raw",Year,SU,sep = '/'))
dirs <- list.dirs()

### Are there any Data subdirectories in this SU?
if(any(str_detect(dirs,"Data"))){
  tmp1 <- dirs[str_detect(dirs,'Data')]
  tmp2 <- str_extract(tmp1,"[^/]+(?=/Data)")
  message("This SU needs to be organized")
}else{
  message("This SU does not contain 'Data' subdirectories")
}

### Copy files from the Data subdirectory to the StationLocation directory
### Delete the 'Data' subdirectory
for(i in 1:length(tmp2)){
  setwd(paste("E:","NABat",State,"Raw",Year,SU,tmp2[i],"Data",sep = '/'))
  files <- list.files()
  file.copy(files,paste("E:","NABat",State,"Raw",Year,SU,tmp2[i],sep = '/'))
  file.remove(files)
  setwd(paste("E:","NABat",State,"Raw",Year,SU,sep = '/'))
  unlink(paste("E:","NABat",State,"Raw",Year,SU,tmp2[i],"Data",sep = '/'),recursive = T)
}