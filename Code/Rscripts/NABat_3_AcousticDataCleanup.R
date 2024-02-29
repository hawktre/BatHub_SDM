library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(lutz)
library(DBI)

# Prep the acoustic data for NABat ----
# The acoustic data used here is the previously compiled vetted SonoBat output files from NABat_1_CompileVettedOutputFiles.R
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the NABat Project corresponding to the acoustic data you want to prep: {NW | ID | USFWS | USFS_Region04}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

## SonoBat Data ----
# Read in compiled Vetted Output
acousticSB <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/AcousticOutput_NABat_',project,'_SonoBat_',year,'.csv'),
                       col_types = cols(.default = 'c'))

# Calculate Location Name based on project
acousticSB <- acousticSB %>% 
  mutate('Location Name' =
           case_when(project == 'NW' ~ LocationNameCONUS,
                     TRUE ~ LocationNameGRTS))

acousticSB <- acousticSB %>% 
  select('GRTS Cell Id' = GRTS,
         `Location Name`,
         SiteDeployment,
         MonitoringNight,
         Filename,
         SppAccp,
         `Species Manual ID`,
         `User|ManualIDSpp2`)

### Audio Recording Name ----
acousticSB <- acousticSB %>% 
  mutate('Audio Recording Name' = Filename)

acousticSB %>% 
  filter(is.na(`Audio Recording Name`) |
           `Audio Recording Name` == '')

### Audio Recording Time ----
acousticSB <- acousticSB %>% 
  mutate('Audio Recording Time' = format(ymd_hms(str_extract(`Audio Recording Name`, "(?<=-).{15}")),'%Y-%m-%d %H:%M:%S'))

acousticSB %>% 
  filter(is.na(`Audio Recording Time`))

acousticSB %>% 
  distinct(`Audio Recording Time`)

### Software Type ----
acousticSB <- acousticSB %>% 
  mutate ('Software Type' = 
            case_when(project == 'USFS_Region04' ~ 'Sonobat 4.4.5',
                      TRUE ~ 'Sonobat 4.2'))

acousticSB %>% 
  distinct(`Software Type`)

### Auto Id ----
# Check SppAccp from vetted data
acousticSB %>% 
  distinct(SppAccp)

# If there's anything weird, fix it
acousticSB <- acousticSB %>% 
  mutate(SppAccp = 
           case_when(SppAccp == 'x' ~ NA_character_,
                     TRUE ~ SppAccp))

# Every file should have an auto id
acousticSB <- acousticSB %>% 
  mutate('Auto Id1' = str_extract(`Audio Recording Name`, '(?<=-)([a-zA-Z]+)(?=\\.)'),
         'Auto Id' = 
           case_when(!is.na(SppAccp) ~ SppAccp,
                     (`Auto Id1` == 'LoF' |
                        `Auto Id1` == 'HiF' |
                        `Auto Id1` == 'noID' |
                        is.na(`Auto Id1`)) ~ 'NoID',
                     TRUE ~ `Auto Id1`)) %>% 
  mutate(`Auto Id` = 
           case_when(`Auto Id` == 'NoID' |
                       `Auto Id` == 'HiLo' ~ `Auto Id`,
                     TRUE ~ toupper(`Auto Id`))) %>% 
  select(-`Auto Id1`)

acousticSB %>% 
  distinct(`Auto Id`)

### Manual Id ----
# Retain ManualID2 for later
ManID <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/DataRequests/BatHub_NABat_Species_Crosswalk.xlsx'))
head(ManID)
names(ManID)[2] <- 'Manual Id'

# Remove Manual Ids that don't have a corresponding NABat value
acousticSB <- acousticSB %>% 
  left_join(ManID, by = c('Species Manual ID' = 'Bat Hub Code')) %>%
  left_join(ManID, by = c('User|ManualIDSpp2' = 'Bat Hub Code')) %>% 
  mutate(`Manual Id.x` = 
           case_when(`Manual Id.x` == '< Delete >' ~ NA_character_,
                     TRUE ~ `Manual Id.x`),
         `Manual Id.y` = 
           case_when(`Manual Id.y` == '< Delete >' ~ NA_character_,
                     TRUE ~ `Manual Id.y`)) %>% 
  rename('Manual Id' = 'Manual Id.x',
         'Manual Id2' = 'Manual Id.y')

# If Manual Id is NA and Manual Id2 is a valid value, add the species to Manual Id, and remove from Manual Id2
acousticSB <- acousticSB %>% 
  mutate(`Manual Id` =
           case_when(is.na(`Manual Id`) &
                       !is.na(`Manual Id2`) ~ `Manual Id2`,
                     TRUE ~ `Manual Id`),
         `Manual Id2` =
           case_when(`Manual Id` == `Manual Id2` ~ NA_character_,
                     TRUE ~ `Manual Id2`))

acousticSB %>% 
  distinct(`Species Manual ID`, `Manual Id`) %>% 
  print(n=Inf)

acousticSB %>% 
  distinct(`Manual Id`,`Manual Id2`) %>% 
  print(n=Inf)

### Manual Vetter ----
acousticSB <- acousticSB %>% 
  mutate('Manual Vetter' = 'NW Bat Hub')

### Format data ----
acousticSB <- acousticSB %>% 
  select(`GRTS Cell Id`,
         `Location Name`,
         SiteDeployment,
         MonitoringNight,
         `Audio Recording Name`,
         `Audio Recording Time`,
         `Software Type`,
         `Auto Id`,
         `Manual Id`,
         `Manual Id2`,
         `Manual Vetter`)

## Zero-crossing data, if applicable ----
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

### Save acousticZC data ----
write_csv(acousticZC,paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanAcousticData_ZC_',project,'_',year,'.csv'))


## Combine acoustic data types ----
# If both full-spectrum and zero-crossing data exist for the project, combine them
acoustic <- acousticSB %>% 
  add_row(acousticZC)

# If only full-spectrum data, just change object name
acoustic <- acousticSB

# Check that there are no invalid GRTS values
acoustic %>% 
  filter(is.na(`GRTS Cell Id`)|
           `GRTS Cell Id` == '') 

# Check that there are no invalid Site values
acoustic %>% 
  filter(is.na(`Location Name`)|
           `Location Name` == '') 

# Check that there are no invalid MonitoringNight values
acoustic %>% 
  filter(is.na(MonitoringNight)|
           MonitoringNight == '') 


### Remove duplicate records, if they exist ----
# Box errors occasionally create duplicate records, identify and remove
# However, be cautious, because if duplicates were created prior to vetting, only one might have Manual Id
# Make sure to remove file without Manual Id
# Check for duplicate records

####### Check project VettedOutput folder to see if GRTS_ID_Database[1] exist. Maybe be the cause of duplicates ##### 

dim(acoustic)
dup <- acoustic %>% 
  group_by(`Location Name`,`Audio Recording Time`) %>% 
  count() %>% 
  filter(n>1) %>% 
  ungroup() %>% 
  select(`Location Name`,`Audio Recording Time`)

dup %>% 
  distinct(`Location Name`)

# If not duplicates exist, skip ahead to Manual Id2 section

# Investigate each Location/Time duplicate
acoustic %>% 
  filter(paste(`Location Name`,`Audio Recording Time`,sep = '_') %in% paste(dup$`Location Name`,dup$`Audio Recording Time`,sep = '_')) %>% 
  arrange(`Location Name`,`Audio Recording Time`) %>% 
  View()

# Do any duplicates have different Auto Id?
dup$AutoIds <- NA_integer_
for(i in 1:length(dup$`Location Name`)){
  dup$AutoIds[i] <- acoustic %>% 
    filter(`Location Name` == dup$`Location Name`[i] &
             `Audio Recording Time` == dup$`Audio Recording Time`[i]) %>% 
    distinct(`Auto Id`) %>% 
    pull() %>% 
    length()
}
dup %>% 
  distinct(AutoIds)


# Do any duplicates have different Manual Id?
dup$ManualIds <- NA_integer_
for(i in 1:length(dup$'Location Name')){
  dup$ManualIds[i] <- acoustic %>% 
    filter('Location Name' == dup$'Location Name'[i] &
             `Audio Recording Time` == dup$`Audio Recording Time`[i] &
             !is.na(`Manual Id`)) %>% 
    distinct(`Manual Id`) %>% 
    pull() %>% 
    length()
}
dup %>% 
  distinct(ManualIds)

# Investigate all duplicate files with different Manual Id values
tmp1 <- dup %>% 
  filter(ManualIds==2)
acoustic %>% 
  filter(paste('Location Name',`Audio Recording Time`,sep = '_') %in% paste(tmp1$'Location Name',tmp1$`Audio Recording Time`,sep = '_')) %>% 
  View()


# Add Audio Recording Name to use to remove duplicate files
dup$'Audio Recording Name' <- NA_character_

# For 106511_NW1 7/22/2022 00:41:01 duplicate files, remove the one with Manual Id = LACITABR
#dup$`Audio Recording Name`[dup$Site == '3738_SE2' &
                             #dup$`Audio Recording Time` == ymd_hms('20220722_004101')] <- '106511_NW1-20220722_004101[1]-Laci.wav'

# Loop through duplicates and identify which file to remove
for(i in 1:length(dup$'Location Name')){
  tmp1 <- acoustic %>% 
    filter('Location Name' == dup$'Location Name'[i] &
             `Audio Recording Time` == dup$`Audio Recording Time`[i])
  tmp2 <- unique(tmp1$`Manual Id`[!is.na(tmp1$`Manual Id`)])
  length(tmp2)
  # If none have Manual Id, remove the file with '[1]' in the name
  if(length(tmp2)==0){
    dup$`Audio Recording Name`[i] <- tmp1$`Audio Recording Name`[str_detect(tmp1$`Audio Recording Name`,'\\[1\\]')]
  }else{
    # If one of the files has a Manual Id, remove the one without
    if(length(tmp2)==1){
      dup$`Audio Recording Name`[i] <- tmp1$`Audio Recording Name`[is.na(tmp1$`Manual Id`)]
    }
  }
}

# Check duplicate list
dup %>% 
  print(n=Inf)


# Check that the duplicate files we are planning to remove are the correct ones, i.e. don't have Manual Id
acoustic %>% 
  filter(`Audio Recording Name` %in% dup$`Audio Recording Name`) %>% 
  print(n=Inf)

# Remove duplicate files
acoustic <- acoustic %>% 
  filter(!`Audio Recording Name` %in% dup$`Audio Recording Name`)

# Check to ensure no more duplicates
acoustic %>% 
  group_by(`Location Name`,`Audio Recording Time`) %>% 
  count() %>% 
  filter(n>1) %>% 
  select(`Location Name`,`Audio Recording Time`)

dim(acoustic)


### Add records for Manual Id2 ----
# NABat only accepts one Manual Id per audio file
# Are there any Manual Id2 species that should be included?
# Any Site-ManualID2 that don't occur in Site-ManualID?
tmp1 <- acoustic %>% 
  filter(!paste0(`Location Name`,'-',SiteDeployment,'-',`Manual Id2`) %in% paste0(`Location Name`,'-',SiteDeployment,'-',`Manual Id`) &
           !is.na(`Manual Id2`))


# Loop through tmp1, if Manual Id exists elsewhere in the deployment copy Manual Id2 into Manual Id,
# if Manual Id doesn't exists elsewhere in the deployment add a new row in acoustic with Manual Id2 as Manual Id,
# and add '(copy)' to filename
for(i in 1:length(tmp1$`Location Name`)){
  tmp2 <- acoustic %>% 
    filter(`Location Name` == tmp1$`Location Name`[i] &
             SiteDeployment == tmp1$SiteDeployment[i] &
             `Audio Recording Name` != tmp1$`Audio Recording Name`[i])
  if(tmp1$`Manual Id`[i] %in% tmp2$`Manual Id`){
    acoustic <- acoustic %>% 
      mutate(`Manual Id` = 
               case_when(`Audio Recording Name` == tmp1$`Audio Recording Name`[i] ~ tmp1$`Manual Id2`[i],
                         TRUE ~ `Manual Id`))
  }else{
    tmp1$`Manual Id`[i] <- tmp1$`Manual Id2`[i]
    tmp1$`Manual Id2`[i] <- NA_character_
    tmp1$`Audio Recording Name`[i] <- str_replace(tmp1$`Audio Recording Name`[i],'.wav','(copy).wav')
    acoustic <- acoustic %>% 
      add_row(tmp1[i,])}
}

acoustic <- acoustic %>% 
  select(-`Manual Id2`)

### Save combined acoustic data ----
write_csv(acoustic,paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanAcousticData_',project,'_',year,'.csv'))

