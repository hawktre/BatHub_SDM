library(tidyverse)
library(lubridate)
library(MMWRweek)
library(readr)

station <- 'BLM_Vale'
cell <- '99610'
setwd(paste0('E:/YearRoundMonitoring/Oregon/Processed/',station,'_',cell,'_YR'))

tmp1 <- list.files(pattern = '*.wav', ignore.case = T, recursive = T)
length(tmp1)
length(tmp1[str_detect(tmp1,'^20')])
length(tmp1[str_detect(tmp1,'^BLM')])
#tmp1 <- tmp1[!str_detect(tmp1,'^20')]

tmp2 <- data.frame(Path = tmp1)
tmp2 <- tmp2 %>% 
  mutate(Time = force_tz(ymd_hms(str_extract(Path, '\\d{8}_\\d{6}')), tzone = 'America/Los_Angeles'))

tmp2 <- tmp2 %>% 
  mutate(Night = 
           case_when(format(Time, '%H:%M:%S') > '12:00:00' ~ as.Date(Time, tz = 'America/Los_Angeles'),
                       TRUE ~ as.Date(Time, tz = 'America/Los_Angeles') - 1))

tmp2 <- tmp2 %>% 
  mutate(Week = epiweek(Night),
         Year = case_when(Week > 50 & month(Night) == 1 ~ year(Night) - 1,
                          Week < 5 & month(Night) == 12 ~ year(Night) + 1,
                          TRUE ~ year(Night)))


srvy.wks <- tmp2 %>% 
  distinct(Year,Week) %>% 
  mutate(Path = paste0(getwd(),'/',Year,'_',Week),
         Station = station,
         CONUS = cell,
         weekStartNight = MMWRweek2Date(Year,Week),
         weekEndNight = MMWRweek2Date(Year,Week)+6) %>% 
  select(4,5,2,1,6,7,3)

### Loop through each epiweek, create a new folder, and copy data to the associated epiweek folder
for(i in 1:length(srvy.wks$Week)){
  dir.create(paste0(srvy.wks$Year[i], '_', srvy.wks$Week[i]))
  files <- tmp2 %>% 
    filter(Year == srvy.wks$Year[i] &
             Week == srvy.wks$Week[i]) %>% 
    pull(Path)
  files.new <- paste0(srvy.wks$Year[i], '_', srvy.wks$Week[i],'/',str_extract(files, '[^/]+[Ww][Aa][Vv]'))
  file.copy(files,files.new)
  }


#######################################################
### Count number of acoustic files for each epiweek ###
#######################################################
### BatIDs in processed data
bats <- c("Anpa",
          "Coto",
          "Epfu",
          "Euma",
          "Laci",
          "Lano",
          "Myca", 
          "Myci",
          "Myev",
          "Mylu",
          "Myth",
          "Myvo",
          "Myyu",
          "Pahe",
          "Tabr")

classifier <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Software/scripts/C10KClassifier.csv')
classifier <- classifier[,c(1,3)]
names(classifier) <- c('CONUS','Classifier')
classifier$CONUS <- as.character(classifier$CONUS)

srvy.wks <- srvy.wks %>% 
  left_join(classifier)


for(i in 1:length(srvy.wks$Week)){
  setwd(srvy.wks$Path[i])
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  tmp2 <- str_extract(tmp1, '(?<=-)[^\\d]+(?=\\.)')
  srvy.wks$filesAutoClassified[i] <- length(tmp1)
  srvy.wks$filesBatID[i] <- length(tmp2[tmp2 %in% bats])
  tmp3 <- ymd_hms(str_extract(tmp1, '\\d{8}_\\d{6}'))
  srvy.wks$fileFirst[i] <- as.character(min(tmp3))
  srvy.wks$fileLast[i] <- as.character(max(tmp3))
}

srvy.wks %>% 
  summarise(diff = max(difftime(ymd_hms(fileLast), ymd_hms(fileFirst), units='days')))

write_csv(srvy.wks,
          paste0('E:/YearRoundMonitoring/VettingTracker_', cell, '_', format(Sys.Date(),'%Y%m%d'), '.csv'))
