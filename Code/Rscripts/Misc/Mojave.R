library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

park <- 'DEVA'
park <- 'GRBA'  # Only ran once through Kaleidoscope, not sansLABO
park <- 'JOTR'
park <- 'LAKE'
park <- 'MOJA'
park <- 'PARA'

datSono <- read.delim(paste0('F:/MOJN_Bats/2021_MOJN_Raw/MOJN_2021_Winter_Processed/', park, '/', park, '_CumulativeSonoBatch_v4.4.5.txt'), colClasses = c('character'), check.names = FALSE)
aSono <- names(datSono)
datSono <- read.delim(paste0('F:/MOJN_Bats/2021_MOJN_Raw/MOJN_2021_Winter_Processed/', park, '/', park, '_CumulativeSonoBatch_v4.4.5.txt'), colClasses = c('character'))
head(datSono)
dim(datSono)

datKal <- read.csv(paste0('F:/MOJN_Bats/2021_MOJN_Raw/MOJN_2021_Winter_Processed/', park, '/Kal_Output/id.csv'), colClasses = c('character'), check.names = F)
aKal <- names(datKal)[16:22]
datKal <- read.csv(paste0('F:/MOJN_Bats/2021_MOJN_Raw/MOJN_2021_Winter_Processed/', park, '/Kal_Output/id.csv'), colClasses = c('character'))
head(datKal)
dim(datKal)

datKal_sansLABO <- read.csv(paste0('F:/MOJN_Bats/2021_MOJN_Raw/MOJN_2021_Winter_Processed/', park, '/Kal_Output_sansLABO/id.csv'), colClasses = c('character'))
head(datKal_sansLABO)
dim(datKal_sansLABO)


##############################################
### Format CumulativeSonoBatch__v4.4.5.txt ###
##############################################
### CumulativeSonoBatch file has wonky alignment
### There is not an easy way to insert value and shift right in R, but this combersome script does the same thing, row by row
### Only a few rows line up properly
datSono %>% 
  filter(NextDirUp == park)

del <- NULL
for(i in 1:length(datSono$Filename)){
  if(datSono$NextDirUp[i] == park){
  }else{
    tmp1 <- datSono[i,]
    tmp2 <- which(tmp1 == park)
    if(length(tmp2) == 0){
      del <- c(del,i)}else{
    if(tmp2 == 24){
      tmp3 <- as.character(tmp1[,1:22])
      tmp4 <- as.character(tmp1[,23:30])
      tmp5 <- rep('',4)
      datSono[i,] <- c(tmp3,tmp5,tmp4)}
    if(tmp2 == 25){
      tmp3 <- as.character(tmp1[,1:23])
      tmp4 <- as.character(tmp1[,24:31])
      tmp5 <- rep('',3)
      datSono[i,] <- c(tmp3,tmp5,tmp4)}
    if(tmp2 == 26){
      tmp3 <- as.character(tmp1[,1:24])
      tmp4 <- as.character(tmp1[,25:32])
      tmp5 <- rep('',2)
      datSono[i,] <- c(tmp3,tmp5,tmp4)}
    if(tmp2 == 27){
      tmp3 <- as.character(tmp1[,1:25])
      tmp4 <- as.character(tmp1[,26:33])
      tmp5 <- rep('',1)
      datSono[i,] <- c(tmp3,tmp5,tmp4)}
    if(tmp2 == 29){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,28:34])
      tmp5 <- c('32')
      datSono[i,] <- c(tmp3,tmp4,tmp5)}
    if(tmp2 == 30){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,29:34])
      tmp5 <- c('0.20', '32')
      datSono[i,] <- c(tmp3,tmp4,tmp5)}
    if(tmp2 == 31){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,30:34])
      tmp5 <- c('0.08','0.20', '32')
      datSono[i,] <- c(tmp3,tmp4,tmp5)}
    if(tmp2 == 32){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,31:34])
      tmp5 <- c('','0.08','0.20', '32')
      datSono[i,] <- c(tmp3,tmp4,tmp5)}
    if(tmp2 == 33){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,32:34])
      tmp5 <- c('4.4.5','','0.08','0.20', '32')
      datSono[i,] <- c(tmp3,tmp4,tmp5)}
      }
  }
}
datSono <- datSono %>% 
  filter(!row_number() %in% del)


### Make sure the above script corrected all alignment issues
datSono %>% 
  filter(NextDirUp != park)


### CumulativeSonoBatch files not in Kaleidoscope output
datSono %>% 
  filter(!(str_extract(Filename, '.+_\\d{6}') %in% str_extract(datKal$IN.FILE, '.+_\\d{6}')))

### Kaleidoscope files not in CumulativeSonoBatch output
datKal %>% 
  filter(!(str_extract(IN.FILE, '.+_\\d{6}') %in% str_extract(datSono$Filename, '.+_\\d{6}')))


################################################
### Combine SonoBat and Kaleidoscope outputs ###
################################################
### Since File names do not match exactly (CumulativeSonoBatch filename does not include -noID, -LoF, -HiF),
### Create a column as a substring of file name to join by
datSono$joinBy <- str_extract(datSono$Filename, '.+_\\d{6}')
datKal$joinBy <- str_extract(datKal$IN.FILE, '.+_\\d{6}')
datKal_sansLABO$joinBy <- str_extract(datKal_sansLABO$IN.FILE, '.+_\\d{6}')
head(datSono)
head(datKal)
head(datKal_sansLABO)
dim(datSono)
dim(datKal)
dim(datKal_sansLABO)

### For Parks without sansLABO Kaleidoscope output, comment out the second full_join()
data <- datSono %>% 
  full_join(datKal %>% 
              select(joinBy,IN.FILE,AUTO.ID.,PULSES,MATCHING,MATCH.RATIO,MARGIN,ALTERNATE.1,ALTERNATE.2), by = 'joinBy') %>% 
  full_join(datKal_sansLABO %>% 
              select(joinBy,AUTO.ID.,PULSES,MATCHING,MATCH.RATIO,MARGIN,ALTERNATE.1,ALTERNATE.2), by = 'joinBy') %>% 
  mutate(Filename =
           case_when(is.na(Filename) ~ IN.FILE,
                     TRUE ~ Filename)) %>% 
  select(-joinBy, -IN.FILE)
dim(data)
head(data)
data %>% 
  filter(is.na(Path))
#names(data) <- c(aSono, paste0('Kal_', aKal))
names(data) <- c(aSono, paste0('Kal_', aKal), paste0('Kal_sansLABO_', aKal))

#write.csv(data, paste0('F:/MOJN_Bats/2021_MOJN_Raw/MOJN_2021_Winter_Processed/', park, '/CombinedOutput_', park, '.csv'), row.names = F)



#########################################
### Acoustic Data vetted by Kirk Navo ###
#########################################

### Summer Data
setwd('G:/Analysis Results Database/MOJN_2021_Summer')
setwd('C:/Users/emblidgp/Downloads/SummerAnalysis')
#a <- list.dirs(full.names = F,recursive = F)

### Run through this script for each park individually
#park <- 'DEVA'
#park <- 'GRBA'
#park <- 'JOTR'
park <- 'LAKE'
#park <- 'MOJA'
#park <- 'PARA'

dat <- list.files(path = paste0(getwd(), "/", park), pattern = '*[Dd]atabase.txt', full.names = TRUE) %>% 
  lapply(read_tsv, col_types = cols(.default = 'c')) %>% 
  bind_rows

#write.csv(dat, paste0(park, '_Summer_2021_Combined.csv'), na = '', row.names = F)

dat1 <- dat %>% 
  select(ParentDir,`Species Manual ID`,`User|ManualIDSpp2`) %>% 
  rename('Site' = 'ParentDir') %>% 
  pivot_longer(cols = contains("Manual", ignore.case = T),
               names_to = NULL,
               values_to = "ManualID") %>% 
  mutate(ManualID = toupper(ManualID))

dat1 %>% 
  distinct(ManualID)


### Compare vetted species to the known species list for the park
library(readxl)
bats <- read_excel(paste0("C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/NPS_Mojave/NPSpecies_FullList_", park, ".xlsx"), sheet = 'Sheet2')

dat1 %>% 
  left_join(bats, by = c('ManualID' = 'Code4')) %>% 
  distinct(ManualID,Occurrence)

### Investigate species IDs that are not on Park species list


### Create species presence table, including all sites with acoustic data, not just those with identified bats. 
dat1 <- dat1 %>% 
  mutate(Detected = 1)

dat1.richness <- dat1 %>% 
  pivot_wider(id_cols = Site, names_from = ManualID, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>% 
  select(-`NA`) %>% 
  arrange(Site)

#write.csv(dat1.richness, paste0(park, '_Summer_2021_Richness.csv'), na = '', row.names = FALSE)



### Winter Data
setwd('G:/Analysis Results Database/MOJN_2021_Winter')
setwd('C:/Users/emblidgp/Downloads/WinterAnalysis')

a <- list.dirs(full.names = F,recursive = F)

for(i in 1:length(a)){
  dat <- list.files(path = paste0(getwd(), "/", a[i]), pattern = '[Dd]atabase.*\\.txt', full.names = TRUE) %>% 
    lapply(read_tsv, col_types = cols(.default = 'c')) %>% 
    bind_rows
  
  write.csv(dat, paste0(str_extract(a[i], '.{4}'), '_Winter_2021_Combined.csv'), na = '', row.names = F)
  
  dat1 <- dat %>% 
    select(ParentDir,`Species Manual ID`,`User|ManualIDSpp2`) %>% 
    rename('Site' = 'ParentDir')
  t1 <- dat1[,1:2]
  names(t1)[2] <- 'ManualID'
  t2 <- dat1[,c(1,3)]
  names(t2)[2] <- 'ManualID'
  dat1 <- rbind(t1,t2)
  
#  dat1 %>% 
#    distinct(ManualID)
  
  dat1 <- dat1 %>% 
    mutate(ManualID = toupper(ManualID))
  
  dat1 <- dat1 %>% 
    mutate(Detected = 1)
  
  dat1.richness <- dat1 %>% 
    pivot_wider(id_cols = Site, names_from = ManualID, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>%
    select(-`NA`) %>% 
    arrange(Site)
  
  write.csv(dat1.richness, paste0(str_extract(a[i], '.{4}'), '_Winter_2021_Richness.csv'), na = '', row.names = FALSE)}
