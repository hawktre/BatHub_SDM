library(tidyverse)

setwd('C:/Users/emblidgp/Downloads/SitePhotosNV')

files <- list.files()

# SitePhotos without proper Station Location in filename
files[is.na(str_extract(files,'\\d+_[NS][EW]\\d'))]

# Station Locations with more than 1 file 
data.frame(StationLocation = str_extract(files,'\\d+_[NS][EW]\\d')) %>% 
  group_by(StationLocation) %>% 
  count() %>% 
  filter(n>1) %>% 
  print(n=Inf)


### Create match table
setwd('C:/Users/emblidgp/Desktop/SitePhotos_FieldMaps2023')

CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

match.table <- data.frame(Site = str_extract(list.files(recursive = T),'\\d+_[NS][EW]\\d'),
                          Photo = paste0(getwd(),'/',list.files(recursive = T))) %>% 
  mutate(SU = as.numeric(str_extract(Site,"\\d+"))) %>% 
  left_join(CellList, by = c('SU' = 'GRTS_ID')) %>% 
  mutate(Site_CONUS =
           case_when(str_detect(Photo,"/ID/") ~ str_replace(Site,"\\d+",as.character(CONUS_10KM)),
                     TRUE ~ Site)) %>% 
  select(Site_CONUS,Photo)

write_csv(match.table,"C:/Users/emblidgp/Desktop/SitePhotos_FieldMaps2023/MatchTable.csv")
