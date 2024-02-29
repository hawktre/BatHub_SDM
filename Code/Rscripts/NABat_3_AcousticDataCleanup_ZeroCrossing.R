library(tidyverse)
library(readxl)
library(stringr)

onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the NABat Project corresponding to the acoustic data directory you want to compile: {NW | ID | USFWS | USFS_Region04}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

### Read in combined ZC output from KaleidoscopeResults and NABat_1_CompileVettedOutputFiles.R 

acousticZC <- read_csv('C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/AcousticOutput_NABat_Kaleidoscope_2023.csv')
head(acousticZC)
View(acousticZC)

### Original processing should have removed Noise, double check
acousticZC %>% 
  distinct(AUTO.ID.)

### Format
acousticZC <- acousticZC %>% 
  select(GRTS,
         Site, CONUS_10KM,DATE,
         IN.FILE,
         AUTO.ID.,
         MANUAL.ID)


### Audio Recording Name
acousticZC <- acousticZC %>% 
  mutate('Audio Recording Name' = paste0(Site, '_', IN.FILE))

acousticZC %>% 
  distinct(`Audio Recording Name`)


### Audio Recording Time
acousticZC <- acousticZC %>% 
  mutate('Audio Recording Time' = ymd_hms(str_extract(`Audio Recording Name`, "(?<=[NS][EW]\\d_).{19}")))

acousticZC %>% 
  filter(is.na(`Audio Recording Time`))


### Software Type
acousticZC <- acousticZC %>% 
  mutate ('Software Type' = 'Kaleidoscope 5 x')


### Auto Id
### Check AUTO.ID. against NABat Template values
nab <- c('25k','40k','40kMyo','ANPA','ANPAEPFU','ANTPAL','ARJA','ARTJAM','BRACAV','BRCA','CHME','CHOMEX','CORA',
         'CORRAF','CORTO','COTO','COTOVI','DIEC','DIPECA','EPFU','EPFULABO','EPFULANO','EPFUMYLU','EPTFUS','EUDMAC',
         'EUFL','EUMA','EUMFLO','EUMPER','EUMUND','EUPE','EUUN','HiF','HighF','IDIPHY','IDPH','LABL','LABLPAHE',
         'LABO','LABOLASE','LABOMYLU','LABONYHU','LABOPESU','LACI','LACILANO','LACITABR','LAEG','LAIN','LAMI',
         'LANO','LANOTABR','LASBLO','LASBOR','LASCIN','LASE','LASEGA','LASINT','LASMIN','LASNOC','LASSEM','LASXAN',
         'LAXA','LEMY','LENI','LEPNIV','LEPYER','LESP','LEYE','LUSO','LoF','LowF','MACA','MACCAL','MOLMOL','MOME',
         'MOMO','MORMEG','MYAR','MYAU','MYCA','MYCAMYCI','MYCAMYYU','MYCI','MYCIMYVO','MYEV','MYEVMYTH','MYGR',
         'MYKE','MYLE','MYLU','MYLUMYCI','MYLUMYSE','MYLUMYVO','MYOAUR','MYOAUS','MYOC','MYOCAL','MYOCIL','MYOEVO',
         'MYOGRI','MYOKEE','MYOLEI','MYOLUC','MYOOCC','MYOSEP','MYOSOD','MYOTHY','MYOVEL','MYOVOL','MYOYUM','MYSE',
         'MYSO','MYTH','MYVE','MYVO','MYYU','NOCLEP','NOISE','NOLE','NOTBAT','NYCFEM','NYCHUM','NYCMAC','NYFE',
         'NYHU','NYMA','NYSP','NoID','PAHE','PARHES','PERSUB','PESU','STERUF','STRU','TABR','TADBRA')
acousticZC %>% 
  filter(!(AUTO.ID. %in% nab)) %>% 
  distinct(AUTO.ID.)

# Every file should have an auto id
acousticZC <- acousticZC %>% 
  mutate('Auto Id' = 
           case_when(AUTO.ID. == 'CORTOW' ~ 'COTO',
                     TRUE ~ AUTO.ID.))
acousticZC %>% 
  filter(!(`Auto Id` %in% nab)) %>% 
  distinct(`Auto Id`)


### Manual Id
# Check to make sure all MANUAL.ID values are in the list of accepted values on the Bulk Upload Template
acousticZC %>% 
  filter(!(MANUAL.ID %in% nab)) %>% 
  distinct(MANUAL.ID)

acousticZC %>% 
  distinct(MANUAL.ID)

acousticZC <- acousticZC %>% 
  mutate(`Manual Id` =
           case_when(MANUAL.ID == 'pMYOVOL' ~ NA_character_,
                     TRUE ~ MANUAL.ID))

acousticZC %>% 
  filter(!(`Manual Id` %in% nab)) %>% 
  distinct(`Manual Id`)

acousticZC %>% 
  distinct(MANUAL.ID, `Manual Id`)


### Format ZC data
acousticZC <- acousticZC %>% 
  mutate('Manual Vetter' = 'NW Bat Hub',
         'GRTS Cell Id' = paste0(GRTS),
         'Location Name' = paste0(Site),
         SiteDeployment = 1,
         MonitoringNight = paste0(DATE))


### Select columns
acousticZC <- acousticZC %>% 
  select('GRTS Cell Id',
         'Location Name', SiteDeployment, MonitoringNight,
         `Audio Recording Name`,
         `Audio Recording Time`,
         `Software Type`,
         `Auto Id`,
         `Manual Id`,
         `Manual Vetter`)

### Save formatted acousticZC data ----
write_csv(acousticZC,paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanAcousticData_ZC_',project,'_',year,'.csv'))




### Combine full spectrum and zero cross data
csv_file1 <- "C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/NABat_CleanAcousticDataTIMEFORMAT_ID_2023.csv"
csv_file2 <- "C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/NABat_CleanAcousticDataTIMEFORMAT_ZC_ID_2023.csv"

acousticSB <- read.csv(csv_file1)
acousticZC <- read.csv(csv_file2)

combined_data <- dplyr::bind_rows(acousticSB, acousticZC)

write.csv(combined_data,"C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/NABat_CleanAcousticData_CombinedTIMEFORMAT_ID_2023.csv" , row.names = FALSE)





###############################################################################################
# Read in Cell List for CONUS and GRTS calculation
CellList <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx'), col_types = c('text')) %>% 
  select(1:2)

# Calculate GRTS and CONUS for each file
acoustic.zc <- acoustic.zc %>% 
  left_join(CellList, by = c('Cell' = 'CONUS_10KM')) %>% 
  left_join(CellList, by = c('Cell' = 'GRTS_ID'))
  


