library(tidyverse)
### Compiling vetting output into a single file
### Copy all vetted files from Box to C:\Users\emblidgp\Desktop\DataImport\VettedData


##############
### Oregon ###
##############
#setwd('C:/Users/emblidgp/Desktop/DataImport/VettedData/OR')
setwd('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_CoosBay_105579_YR/VettingOutput')
files <- list.files()
files
acoustic_CoosBay <- NULL
for(i in 1:length(files)){
  tmp1 <- read.delim(files[i], colClasses = c('character'))
  acoustic_CoosBay <- rbind(acoustic_CoosBay,tmp1)}

setwd('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_Klamath_99113_YR/VettingOutput')
files <- list.files()
files
acoustic_Klamath <- NULL
for(i in 1:length(files)){
  tmp1 <- read.delim(files[i], colClasses = c('character'))
  acoustic_Klamath <- rbind(acoustic_Klamath,tmp1)}

setwd('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_Salem_113928_YR/VettingOutput')
files <- list.files()
files
acoustic_Salem1 <- NULL
for(i in 1:length(files)){
  tmp1 <- read.delim(files[i], colClasses = c('character'))
  acoustic_Salem1 <- rbind(acoustic_Salem1,tmp1)}

setwd('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_Salem_113929_YR/VettingOutput')
files <- list.files()
files
acoustic_Salem2 <- NULL
for(i in 1:length(files)){
  tmp1 <- read.delim(files[i], colClasses = c('character'))
  acoustic_Salem2 <- rbind(acoustic_Salem2,tmp1)}

setwd('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_Vale_99610_YR/VettingOutput')
files <- list.files()
files
acoustic_Vale <- NULL
for(i in 1:length(files)){
  tmp1 <- read.delim(files[i], colClasses = c('character'))
  acoustic_Vale <- rbind(acoustic_Vale,tmp1)}


### Combine all SonoBat outputs
acoustic <- acoustic_CoosBay %>% 
  add_row(acoustic_Klamath) %>% 
  add_row(acoustic_Salem1) %>% 
  add_row(acoustic_Salem2) %>% 
  add_row(acoustic_Vale)
dim(acoustic)

write.csv(acoustic, 'C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_output.csv', na = '', row.names = FALSE, quote = FALSE)


head(acoustic)

acoustic <- acoustic %>% 
  select(NextDirUp,ParentDir,Species.Manual.ID,User.ManualIDSpp2)
t1 <- acoustic[,1:3]
names(t1)[3] <- 'ManualID'
t2 <- acoustic[,c(1:2,4)]
names(t2)[3] <- 'ManualID'
dat <- rbind(t1,t2)


dat <- dat %>% 
  rename('Station' = 'NextDirUp',
         'Week' = 'ParentDir')


dat %>% 
  distinct(ManualID)

### Convert Code6 to Code4
data2021 <- data2021 %>% 
  mutate(ManualID =
           case_when(ManualID == 'LASCIN' ~ 'LACI',
                     ManualID == 'LASNOC' ~ 'LANO',
                     ManualID == 'MYOCIL' ~ 'MYCI',
                     ManualID == 'MYOEVO' ~ 'MYEV',
                     ManualID == 'MYOLUC' ~ 'MYLU',
                     ManualID == 'MYOYUM' ~ 'MYYU',
                     ManualID == 'MYOCAL' ~ 'MYCA',
                     ManualID == 'EPTFUS' ~ 'EPFU',
                     ManualID == 'MYOTHY' ~ 'MYTH',
                     TRUE ~ ManualID))


bats <- read.csv('C:/Users/emblidgp/Desktop/BatList.csv')

dat <- dat %>% 
  mutate(Detected = 1) %>% 
  filter(ManualID %in% bats$Code)

dat.richness <- dat %>% 
  pivot_wider(id_cols = c(Station,Week), names_from = ManualID, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>% 
  arrange(Station,Week)

data2021.abundance <- data2021 %>% 
  pivot_wider(id_cols = c(State,CONUS10K,GRTS,Site), names_from = ManualID, values_from = Detected, values_fn = sum, values_fill = 0, names_sort = T) %>% 
  arrange(State,GRTS,Site)

write.csv(dat.richness, 'C:/Users/emblidgp/Box/HERS_BatAcousticFIles/Year Round Monitoring/Oregon/Processed/BLM_SppRichness2021.csv', na = '', row.names = FALSE, quote = FALSE)
#write.csv(data2021.abundance, 'C:/Users/emblidgp/Desktop/SpeciesOccurrence/SppAbundance2021.csv', na = '', row.names = FALSE, quote = FALSE)

