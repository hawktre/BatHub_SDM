### New check of detector internal clock
### This concept should be included in DetectorCheck

library(tidyverse)
library(readxl)
library(lubridate)

state <- 'Oregon'
ST <- 'OR'
year <- '2023'

setwd(paste0("E:/NABat/", state, "/Raw/",year))


##################################################
### List all paths to sites within a directory ###
##################################################
### First list all directories
a <- list.dirs(full.names = F)

### Extract path to quad folder
quad.folders <- str_extract(a, '.*\\d+_.+\\d[^/]*')
quad.folders <- na.omit(unique(quad.folders))


CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx', col_types = c('text')) %>% 
  select(1:2)


data.acoustic <- data.frame(State = ST,
                            Path = paste0(getwd(), '/', quad.folders),
                            stringsAsFactors = F) %>% 
  mutate(cell = str_extract(Path, '\\d+(?=_)')) %>% 
  left_join(CellList, by = c('cell' = 'CONUS_10KM')) %>% 
  left_join(CellList, by = c('cell' = 'GRTS_ID')) %>% 
  mutate(CONUS =
           case_when(State != 'ID' ~ cell,
                     TRUE ~ CONUS_10KM),
         GRTS = 
           case_when(State == 'ID' ~ cell,
                     TRUE ~ GRTS_ID),
         LocationNameCONUS = paste0(CONUS,str_extract(Path, '_[NS][EW]\\d')),
         LocationNameGRTS = paste0(GRTS,str_extract(Path, '_[NS][EW]\\d'))) %>% 
  select(-c(3:5))

data.acoustic <- data.acoustic %>% 
  mutate(SiteDeployment = 
           case_when(str_detect(.$Path, '\\(2\\)') ~ 2,
                     TRUE ~ 1))


meta1 <- read_csv(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatSurveyForm',year,'.csv'))
meta <- meta1 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         DeploymentDate = as.Date(mdy_hms(`Deployment Date`), tz = 'America/Los_Angeles'),
         SiteDeployment = 
           case_when(LocationNameCONUS == '110715_NW1' &
                       DeploymentDate == '2023-07-11' ~ 2,
                     LocationNameCONUS == '110715_SW1' &
                       DeploymentDate == '2023-07-11' ~ 2,
                     TRUE ~ 1),
         DetectorSN = as.character(`Detector Serial No.`),
         LocalTime = ymd_hms(str_extract(LocalTime,"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}"))) %>% 
  select(State,
         LocationNameCONUS,
         LocationNameGRTS,
         DeploymentDate,
         SiteDeployment,
         `Detector Type`,
         DetectorSN,
         LocalTime) %>% 
  rename('DetectorType' = 'Detector Type')



dat <- meta %>% 
  left_join(data.acoustic %>% 
              select(LocationNameCONUS,SiteDeployment,Path)) %>% 
  filter(!is.na(Path)) %>% 
  mutate(DetectorON = NA_POSIXct_,
         DetectorSleep = NA_POSIXct_)

for(i in 1:length(dat$State)){
  if(dat$DetectorType[i] == 'D500X' &
     file.exists(paste0(dat$Path[i],'/D500X.LOG'))){
    tmp1 <- read_file(paste0(dat$Path[i],'/D500X.LOG'))
    dat$DetectorON[i] <- ymd_hms(str_extract(tmp1,'.+(?=\\s\\$\\$SYSTEM START)'))
    dat$DetectorSleep[i] <- ymd_hms(str_extract(tmp1,'.+(?=\\s\\$\\$TIMER SLEEP)'))
  }
}

### Detector time programmed by biologist
dat %>% 
  filter(DetectorType == "D500X" &
           abs(DetectorON - DetectorSleep) > minutes(15)) %>% 
  select(State,LocationNameCONUS,DeploymentDate,DetectorSN,LocalTime,DetectorON,DetectorSleep) %>% 
  arrange(desc(abs(DetectorON - DetectorSleep))) %>% 
  print(n=Inf)

# 100990_SW2 52336 6/20/2023 Bio, in correcting time from 1 hour ahead, accidentally set clock 1 year back. File times have been corrected. 
# 100990_NE2 52334 6/21/2023 Can't identify deployment time. Looks like bio set clock back 1 hour at deployment. 
# 99599_NE1  52336 6/21/2023 Bio corrected clock that was accidentally set 1 year back the previous deployment (100990_SW2)
# 105159_SE1 Detector clock reset and started recording right away, then asleep at 06:30 as programmed
# 112536_SW4 All good, SYSTEM START logs from earlier in the day
# 99605_SW6  52334 6/19/2023 Deployed 14:06 PDT. Looks like detector was turned ON at deployment time, but bio may have set clock 1 hour ahead
# 99605_NW1  52169 6/19/2023 Deployed 14:42 PDT. Looks like detector was turned ON at deployment time, but bio may have set clock 1 hour ahead
# 99599_SE3  52169 6/21/2023 Deployed 16:39 PDT. Looks like detector clock was 1 hour ahead when turned on, bio corrected to current local time
# 99605_SW3  52336 6/19/2023 Deployed 12:49 PDT. Looks like detector was turned ON at deployment time, but bio may have set clock 1 hour ahead
# 99605_SW4  51735 6/19/2023 Deployed 13:16 PDT. Clock Reset, but bio may have set clock 1 hour ahead

### 52336 ###
# 99605_SW3  Deployed 6/19/2023 12:49 PDT. Bio set clock to MDT
# 100990_SW2 Deployed 6/20/2023 15:34 PDT. Bio set clock back to correct time, but also set to wrong year (2022)
# 99599_NE1  Deployed 6/21/2023 14:39 PDT. Bio set clock to correct time

### 52334 ###
# 99605_SW6  Deployed 6/19/2023 14:06 PDT. Bio set clock to MDT
# 100990_NE2 Deployed 6/20/2023 16:45 PDT. Redeployment 6/21 clock set back
# 102370_NW3 Deployed 6/27/2023 19:08 PDT. Clock is good

### 52169 ###
# 99605_NW1  Deployed 6/19/2023 14:42 PDT. Bio set clock to MDT
# 100990_NW2 Deployed 6/20/2023 14:56 PDT. Clock still in MDT, should be PDT, corrected file times
# 99599_SE3  Deployed 6/21/2023 16:39 PDT. Bio set clock back to correct time

### 51735 ### Internal battery dead
# 99605_SW4  Deployed 6/19/2023 13:16 PDT. Bio set clock to MDT
# 100990_NW4 Deployed 6/20/2023 18:23 PDT. Bio set clock to PDT
# 99599_SE1  Deployed 6/21/2023 15:53 PDT. NoAcousticData
# 103298_SW1 Deployed 6/29/2023 16:46 PDT. Bio set clock to PDT




### Detector Sleep time different from local time
dat %>% 
  filter(DetectorType == "D500X" &
           abs(LocalTime - DetectorSleep) > minutes(15)) %>% 
  select(State,LocationNameCONUS,DeploymentDate,DetectorSN,LocalTime,DetectorON,DetectorSleep) %>% 
  arrange(desc(abs(LocalTime - DetectorSleep))) %>% 
  print(n=Inf)

# 105159_SE1 6/14/2023 51506 Detector deployed with clock 1/1/2000 00:00:00
# 110715_SW1 7/11/2023 51635 Clock set to year = 2022, time otherwise correct, corrected file names
# 103301_NW1 6/12/2023 51506 Clock 1 year 1 day behind, corrected
# 103301_SE1 6/12/2023 51635 Clock set to year = 2022, time otherwise correct, corrected file names
# 100990_SW2 6/20/2023 52336 Clock set to year = 2022, time otherwise correct, corrected file names
# 110715_SE1 6/26/2023 51635 Clock set to year = 2022, time otherwise correct, corrected file names, Field Maps record not created in situ
# 106544_NW1 6/6/2023  51356 No Acoustic Data, old log
# 106544_SW1 6/6/2023  51637 No Acoustic Data, old log
# 110715_NW1 7/11/2023 51663 Field Maps record not created in situ
# 106544_SE1 6/6/2023  51668 Field Maps record not created in situ
# 106544_NE1 6/6/2023  51672 Field Maps record not created in situ
# 100990_NE2 See above
# 99569_SW2  7/6/2023 51657

tmp4 <- abs(tmp3 - tmp2)
