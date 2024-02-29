library(tidyverse)
library(readxl)

### Sample Units to survey

SUs <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx")

SUs <- SUs %>% 
  filter((is.na(Category) |
            Category != 'Accidental') &
           (grepl('yes',Survey2022,ignore.case = T) |
           grepl('yes',Survey2021,ignore.case = T) |
           grepl('yes',Survey2020,ignore.case = T)) &
           (is.na(`Actively Being Surveyed`) |
              `Actively Being Surveyed` != 'No')) %>% 
  select(CONUS_10KM,GRTS_ID,NABatProject,NABatState,Contact,Agency,Survey2022,Survey2021,Survey2020,Survey2019,Survey2018,Survey2017,Survey2016,Comments,Eval_Notes)

write_csv(SUs,"C:/Users/emblidgp/Desktop/SiteStatus2023/NABat_Target_SUs_2023.csv")
