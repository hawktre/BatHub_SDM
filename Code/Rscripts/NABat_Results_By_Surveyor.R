library(tidyverse)

yr <- '2022'

# Cell Tracker
cells1 <- read_excel(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_',yr,'.xlsx'))
cells2 <- read_excel(paste0('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/FS R4 Call Analysis/Region4_DataReceived',yr,'.xlsx')) %>% 
  filter(!is.na(GRTS_ID)) %>% 
  select(GRTS_ID,Contact,Email,Note2022) %>% 
  mutate('Agency' = "USFS",
         'Completed 2022' = 'Yes',
         'FS R4' = 1)

cells <- cells1 %>% 
  add_row(cells2)

# Convert all NA to ""
cells <- cells %>% 
  mutate(across(c(Contact,Agency,Email),~replace_na(.,"")))

# Cell List
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)


# Species Richness
rich1 <- read_csv(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/',yr,'_Analysis/SppRichness_PNW_',yr,'.csv'))
rich2 <- read_csv(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/',yr,'_Analysis/SppRichness_USFS_Region04_',yr,'.csv'))
rich3 <- read_csv(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/',yr,'_Analysis/SppRichness_USFWS_',yr,'.csv')) %>% 
  select(-`FWS Refuge`)

rich <- rich2 %>% 
  add_row(rich1) %>% 
  add_row(rich3)


# Combine NYSP and NYMA columns
rich <- rich %>% 
  mutate(NYMA = 
           case_when(NYMA == 1 |
                       NYSP == 1 ~ 1,
                     TRUE ~ 0)) %>% 
  select(-NYSP)

# Convert all species NA to 0
rich <- rich %>% 
  mutate(across(ANPA:TABR,~replace_na(.,0)))



rich <- rich %>% 
  left_join(cells %>% 
              select(GRTS_ID,Contact,Agency,Email),
            by = c("GRTS" = "GRTS_ID"))

tmp1 <- cells %>% 
  filter(`Completed 2022` == 'Accidental') %>% 
  mutate(IntendedGRTS = as.numeric(str_extract(Note2022,"(?<=GRTS\\s)\\d+"))) %>% 
  left_join(CellList,
            by = c("IntendedGRTS" = "GRTS_ID")) %>% 
  rename("GRTS" = "GRTS_ID",
         "IntendedCONUS" = "CONUS_10KM.y",
         "CONUS" = "CONUS_10KM.x") %>% 
  select(GRTS,CONUS,`Completed 2022`,Note2022,IntendedGRTS,IntendedCONUS)


# Add CONUS to rich
rich <- rich %>% 
  left_join(CellList,
            by = c("GRTS" = "GRTS_ID")) %>% 
  mutate(SiteGRTS = str_replace(Site,'\\d+',as.character(GRTS)), # Use GRTS ID in Location Name for consistency in reporting.
         SiteCONUS = str_replace(Site,'\\d+',as.character(CONUS_10KM))) %>% 
  rename("CONUS" = "CONUS_10KM") %>% 
  select(GRTS,CONUS,SiteGRTS,SiteCONUS,SiteDeployment:Email)

# Add Site to rich
meta1 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx')
meta1 <- meta1 %>% 
  left_join(CellList,by=c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(`Location Name` = 
           case_when(State == 'ID' ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(GRTS_ID,'_',Quad,`Quad Number`))) %>% 
  select(`Location Name`,`Site Name`)
meta2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx')
meta2 <- meta2 %>% 
  left_join(CellList,by=c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(`Location Name` = 
           case_when(State == 'ID' ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(GRTS_ID,'_',Quad,`Quad Number`))) %>% 
  select(`Location Name`,`Site Name`)
meta3 <- read_excel('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/FS R4 Call Analysis/2022 NABat Data/USFS_Region4_Metadata_2022.xlsx')
meta3 <- meta3 %>% 
  select(`Location Name`,`Site Name`)
meta <- meta1 %>% 
  add_row(meta2) %>% 
  add_row(meta3) %>% 
  filter(!is.na(`Site Name`)) %>% 
  distinct(`Location Name`,`Site Name`)

rich <- rich %>% 
  left_join(meta,by=c('SiteGRTS'='Location Name')) %>% 
  select(GRTS:SiteCONUS,`Site Name`,SiteDeployment:Email)

# Add accidental survey as UnusualOccurrence
if(length(tmp1$GRTS) > 0){
  for(i in 1:length(tmp1$GRTS)){
    rich <- rich %>% 
      mutate(UnusualOccurrences =
               case_when(GRTS == tmp1$GRTS[i] &
                           !is.na(UnusualOccurrences) ~ paste(paste0("Accidental survey intended for GRTS ",tmp1$IntendedGRTS[i]),UnusualOccurrences,sep = ", "),
                         GRTS == tmp1$GRTS[i] &
                           is.na(UnusualOccurrences) ~ paste0("Accidental survey intended for GRTS ",tmp1$IntendedGRTS[i]),
                         TRUE ~ UnusualOccurrences))}}

# Add vetting notes
vet1 <- read_excel('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/FS R4 Call Analysis/2022 NABat Data/VettingTracker_USFS_Region04_2022.xlsx')
vet1 <- vet1 %>% 
  select(Site,SiteDeployment,DeploymentDate,Comments)
vet2 <- read_excel('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/VettingTracker2022.xlsx')
vet2 <- vet2 %>% 
  mutate(Site = paste0(GRTS,str_extract(Site,'_[NS][EW]\\d'))) %>% 
  select(Site,SiteDeployment,DeploymentDate,`Comments for surveyor`) %>% 
  rename('Comments' = 'Comments for surveyor')
vet <- vet1 %>% 
  add_row(vet2) %>% 
  mutate(DeploymentDate = as.Date(DeploymentDate))

rich <- rich %>% 
  left_join(vet,by=c('SiteGRTS'='Site','SiteDeployment')) %>% 
  select(GRTS:SiteDeployment,DeploymentDate,ANPA:Email,Comments) %>% 
  arrange(GRTS,SiteGRTS,SiteDeployment)


tmp1 <- rich %>% 
  distinct(Agency,Contact,Email)

tmp1 %>% 
  print(n=Inf)


for(i in 1:length(tmp1$Agency)){
  tmp2 <- rich %>% 
    filter(Agency == tmp1$Agency[i],
           Contact == tmp1$Contact[i],
           Email == tmp1$Email[i])
  if(length(tmp2$GRTS) > 0){
  write_csv(tmp2 %>% 
              select(-c(Agency,Contact,Email)),
            paste("C:/Users/emblidgp/Desktop/Results2022/NABatResults_2022",str_replace(tmp1$Agency[i],"/",";"),str_replace(tmp1$Contact[i],"/",";"),paste0(tmp1$Email[i],".csv"),sep = "_"),
            na = "")}}

rich %>% 
  filter(is.na(DeploymentDate))
