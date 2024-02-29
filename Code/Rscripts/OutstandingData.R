library(tidyverse)
library(readxl)

meta1 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/NABatSurveyForm2023.csv')
meta2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/NABatMetadata2023_PaperDatasheets.xlsx')

# Combine digital and paper metadata
meta <- meta1 %>% 
  select(State,CONUS,GRTS,Quad,`Quad Number`) %>% 
  add_row(meta2 %>% 
            select(State,CONUS,GRTS,Quad,`Quad Number`)) %>% 
  mutate(LocationNameCONUS = paste0(CONUS,"_",Quad,`Quad Number`),
         LocationNameGRTS = paste0(GRTS,"_",Quad,`Quad Number`))

CellTracker <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx')

meta <- meta %>% 
  left_join(CellTracker %>% 
              select(CONUS_10KM,`BatHub Server`,Box_Raw,NABatProject,NABatState,Contact,Agency),
            by = c("CONUS" = "CONUS_10KM"))

meta %>% 
  filter(is.na(`BatHub Server`)) %>% 
  distinct(CONUS,GRTS,Agency,Contact) %>% 
  arrange(Agency,Contact) %>% 
  print(n=Inf)

### SUs confirmed to be surveyed at the start of the year, but that we don't have data from. 
CellTracker %>% 
  filter(`Confirmed for 2023`=='Yes' &
           is.na(`BatHub Server`) &
           !(CONUS_10KM %in% meta$CONUS)) %>% 
  distinct(CONUS_10KM,GRTS_ID,Agency,Contact) %>% 
  arrange(Agency,Contact) %>% 
  print(n=Inf)
