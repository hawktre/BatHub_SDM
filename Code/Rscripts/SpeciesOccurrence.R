library(tidyverse)
library(readxl)

### Create Sample Unit species richness table
d1 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_PNW_2022.csv')
d2 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_USFWS_2022.csv')

# Combine species richness tables
dat <- d1 %>% 
  add_row(d2 %>% 
            select(-`FWS Refuge`))

# Condense species richness to Sample Unit level
dat <- dat %>% 
  group_by(GRTS) %>% 
  summarise(across(ANPA:TABR,max))

#write_csv(dat, 'C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_SpeciesOccurrence_PNW_2022.csv')


### BatHub Data Viewer
library(tidyverse)
dat <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2021_Analysis/NABat_SpeciesOccurrence_PNW_2016_2021.csv')
head(dat)

dat[dat == "No"] <- '0'
dat[dat == "Yes"] <- '1'

dat <- dat %>% 
  mutate(across(ANPA:TABR,as.numeric))


dat22 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_SpeciesOccurrence_PNW_2022.csv')
head(dat22)

CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx', col_types = 'text') %>% 
  select(1:2)

dat22 <- dat22 %>% 
  left_join(CellList %>% 
              mutate(GRTS_ID = as.numeric(GRTS_ID)),
            by = c('GRTS' = 'GRTS_ID')) %>% 
  select(CONUS_10KM,GRTS,ANPA:TABR) %>% 
  rename('GRTS_ID' = 'GRTS')


### Any new Cells surveyed this year?
dat22 %>% 
  filter(!GRTS_ID %in% dat$GRTS_ID)
    

### Add current year's data to previous
tmpFunc <- function(x){
  x2 <- ifelse(x==0,'No',ifelse(x==1,'Yes',NA_character_))
  return(x2)}

data <- dat %>% 
  add_row(dat22 %>% 
            mutate(CONUS_10KM = as.numeric(CONUS_10KM))) %>% 
  group_by(CONUS_10KM,GRTS_ID) %>% 
  summarise(across(ANPA:TABR,max)) %>% 
  ungroup %>% 
  mutate(Spp_Rich = rowSums(across(ANPA:TABR)),
         across(ANPA:TABR,tmpFunc)) %>% 
  select(CONUS_10KM,GRTS_ID,Spp_Rich,ANPA:TABR)


write_csv(data, 'C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_SpeciesOccurrence_PNW_2016_2022.csv')
