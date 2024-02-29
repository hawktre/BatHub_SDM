library(tidyverse)
library(readxl)

CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx', col_types = 'text') %>% 
  select(1:2)

dat22 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_PNW_2022.csv")
dat22 <- dat22 %>% 
  mutate(GRTS = as.character(GRTS),
         Year = 2022) %>% 
  select(-SiteDeployment,-UnusualOccurrences)

dat21 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2021_Analysis/SppRichness2021.csv")
dat21 <- dat21 %>% 
  mutate(GRTS = as.character(GRTS),
         Year = 2021) %>%
  select(GRTS:Year)

dat20 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2020_Analysis/SppRichness_ORWA_2020.csv")
dat20 <- dat20 %>% 
  mutate(SampleUnit = as.character(SampleUnit),
         Year = 2020) %>% 
  left_join(CellList, by = c('SampleUnit' = 'CONUS_10KM')) %>% 
  mutate(GRTS =
           case_when(SampleUnit == str_extract(LocationID,'\\d+') ~ GRTS_ID,
                     TRUE ~ SampleUnit)) %>% 
  select(GRTS,LocationID:Year) %>% 
  rename('Site' = 'LocationID')

dat19 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2019_Analysis/NABat/2019NWNABat_AnalysisData_20200415_RogerCheck.xlsx", sheet = "Sheet2")
dat19 <- dat19 %>% 
  mutate(CONUS10km = as.character(CONUS10km),
         Year = 2019) %>% 
  select(CONUS10km,Site,ANPA:TABR,Year) %>% 
  left_join(CellList,by = c('CONUS10km' = 'CONUS_10KM')) %>% 
  select(GRTS_ID,Site:Year) %>% 
  rename('GRTS' = 'GRTS_ID') %>% 
  select(-EUPE,-LABL) # These species not detected

dat <- dat22 %>% 
  add_row(dat21) %>% 
  add_row(dat20) %>% 
  add_row(dat19)

tmp1 <- dat %>% 
  filter(str_detect(Site,'115786')) %>% 
  select(Year,GRTS:TABR)

write_csv(tmp1,"C:/Users/emblidgp/Desktop/SppRichness_CONUS115786.csv")
