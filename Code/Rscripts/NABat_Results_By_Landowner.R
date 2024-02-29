library(tidyverse)
library(readxl)

yr <- '2022'

# Landowners/managers who want results
dat1 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Contacts/Landowner_Contacts.xlsx", sheet = "Oregon")
dat2 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Contacts/Landowner_Contacts.xlsx", sheet = "Washington")
dat3 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Contacts/Landowner_Contacts.xlsx", sheet = "Idaho")

landowner <- dat1 %>% 
  add_row(dat2) %>% 
  add_row(dat3) %>% 
  mutate(StationLocationGRTS = 
           case_when(Location == 'All' ~ 'All',
                     TRUE ~ paste0(GRTS,str_extract(Location,'_[NS][EW].*'))),
         StationLocationCONUS = 
           case_when(Location == 'All' ~ 'All',
                     TRUE ~ paste0(CONUS,str_extract(Location,'_[NS][EW].+')))) %>% 
  select(State,Region,GRTS,CONUS,StationLocationGRTS,StationLocationCONUS,Organization:Notes) %>% 
  filter(`Requested Results?`=='Yes')


# Metadata
meta1 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx')
meta1 <- meta1 %>% 
  left_join(CellList,by=c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(GRTS = 
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         `StationLocationGRTS` = paste0(GRTS,'_',Quad,`Quad Number`)) %>% 
  select(GRTS,StationLocationGRTS,x,y) %>% 
  rename('Longitude' = 'x',
         'Latitude' = 'y')
meta2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx')
meta2 <- meta2 %>% 
  left_join(CellList,by=c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(GRTS = 
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         `StationLocationGRTS` = paste0(GRTS,'_',Quad,`Quad Number`)) %>% 
  select(GRTS,StationLocationGRTS,Longitude,Latitude)
meta <- meta1 %>% 
  add_row(meta2)


# Species Richness
rich1 <- read_csv(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/',yr,'_Analysis/SppRichness_PNW_',yr,'.csv'))
rich2 <- read_csv(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/',yr,'_Analysis/SppRichness_USFWS_',yr,'.csv')) %>% 
  select(-`FWS Refuge`)

rich <- rich2 %>% 
  add_row(rich1)

rich <- rich %>% 
  mutate(Site = paste0(GRTS,str_extract(Site,'_.+'))) %>% 
  group_by(GRTS,Site) %>% 
  summarize(across(ANPA:TABR,sum))

data <- meta %>% 
  left_join(rich,by = c('GRTS','StationLocationGRTS' = 'Site'))


tmp1 <- landowner %>% 
  distinct(Organization,Contact1,Contact2,Email1,Email2)

i=5
tmp1[i,]
for(i in 1:length(tmp1$Organization)){
  tmp2 <- landowner %>% 
    filter(Organization == tmp1$Organization[i] &
             Contact1 == tmp1$Contact1[i])
  tmp4 <- NULL
  for(ii in 1:length(tmp2$State)){
    tmp3 <- data %>% 
      {if(str_detect(tmp2$StationLocationGRTS[ii],'All')){
        filter(.,GRTS == tmp2$GRTS[ii])
      }else{
        filter(.,StationLocationGRTS == tmp2$StationLocationGRTS[ii])}}
    tmp4 <- rbind(tmp4,tmp3)}
  if(length(tmp4$GRTS) == 0){
    warning(paste0(paste(tmp2$StationLocationGRTS,collapse = ', '),' has no acoustic results from ',yr))}else{
      write_csv(tmp4,paste("C:/Users/emblidgp/Desktop/Results2022/Landowner/NABatResults_2022",
                           tmp1$Organization[i],
                           if(is.na(tmp1$Contact2[i])){tmp1$Contact1[i]}else{
                             paste(tmp1$Contact1[i],tmp1$Contact2[i],sep = ';')},
                           if(is.na(tmp1$Email2[i])){paste0(tmp1$Email1[i],".csv")}else{
                             paste0(paste(tmp1$Email1[i],tmp1$Email2[i],sep = ';'),".csv")},
                           sep = "_"))}}
names(tmp2)
ii=1

data2 <- NULL
for(i in 1:length(landowner$State)){
  tmp1 <- data %>% 
    {if(str_detect(landowner$StationLocationGRTS[i],'All')){
      filter(.,GRTS == landowner$GRTS[i])
    }else{
      filter(.,StationLocationGRTS == landowner$StationLocationGRTS[i])}}
  if(length(tmp1$GRTS) > 0){
    data2 <- rbind(data2,tmp1)
  }else{
      warning(paste0(landowner$StationLocationGRTS[i],' has no acoustic results from ',yr))
    }
  }


# Return individual files with contact in name ###
i=1
for(i in 1:length(landowner$State)){
  tmp1 <- data2 %>% 
    {if(str_detect(landowner$StationLocationGRTS[i],'All')){
      filter(.,GRTS == landowner$GRTS[i])
    }else{
      filter(.,StationLocationGRTS == landowner$StationLocationGRTS[i])}}
  if(length(tmp1$GRTS) > 0){
    write_csv(tmp1,paste("C:/Users/emblidgp/Desktop/Results2022/Landowner/NABatResults_2022",
                         landowner$Organization[i],
                         if(is.na(landowner$Contact2[i])){landowner$Contact1[i]}else{
                           paste(landowner$Contact1[i],landowner$Contact2[i],sep = ';')},
                         if(is.na(landowner$Email2[i])){paste0(landowner$Email1[i],".csv")}else{
                           paste0(paste(landowner$Email1[i],landowner$Email2[i],sep = ';'),".csv")},
                         sep = "_"))}
}
