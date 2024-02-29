### This script is used to attribute acoustic files by hand
### This is useful for D500X 52336, which was having all acoustic features of WAV files erased when attributed
### Caused by D500x Metadata Mode being set to GUANO, instead of Pettersson

library(tidyverse)

StationLocation <- '95426_SW1'

### Remove Processed files lacking any acoustic information
setwd(paste0("E:/NABat/Oregon/Processed/2023/",str_extract(StationLocation,"\\d+"),"/",StationLocation))
getwd()
file.remove(list.files())

### Copy Raw files to Processed directory
setwd(paste0("E:/NABat/Oregon/Raw/2023/",str_extract(StationLocation,"\\d+"),"/",StationLocation))
file.copy(list.files(pattern = 'WAV'),paste0("E:/NABat/Oregon/Processed/2023/",str_extract(StationLocation,"\\d+"),"/",StationLocation))

### Rename Processed files, basically attributing without SonoBat
setwd(paste0("E:/NABat/Oregon/Processed/2023/",str_extract(StationLocation,"\\d+"),"/",StationLocation))
files <- list.files()

# Extract file timestamp
files2 <- NULL
for(i in 1:length(files)){
  a <- system(paste0("powershell -command \"Select-String -Path ",paste0(getwd(),'/',files[i])," -Pattern 'Timestamp:\\s+.+' | foreach {$_.matches.value} | Get-Unique\""), intern = T)
  b <- a %>% 
    str_extract('(?<=Timestamp: ).+') %>% 
    unique() %>% 
    ymd_hms() %>% 
    format('%Y%m%d_%H%M%S')
  files2 <- c(files2,paste0(StationLocation,"-",b,".wav"))
}

# Rename files
file.rename(files,files2)

### Remove auto-classified species code from file name in preparation for reprocessing entire SU together
StationLocation <- '110256_SW1'
setwd(paste0("E:/NABat/Oregon/Processed/2023/",str_extract(StationLocation,"\\d+"),"/",StationLocation))
files <- list.files()
files2 <- str_remove(files,"(?<=_\\d{6}).+(?=.wav)")

file.rename(files,files2)
