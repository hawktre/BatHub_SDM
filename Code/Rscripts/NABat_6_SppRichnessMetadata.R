# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               readxl)

# Identify yourself, the Project, and year of data to upload ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the NABat Project corresponding to the acoustic data you want to prep: {NW | ID | USFWS | USFS_Region04}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

# Join metadata to Species Richness table ----
## Load metadata and species richness table ----
meta <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanMetadata_',year,'.csv'))
SpRich <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/SppRichness_',project,'_Combined_',year,'.csv'))

## Add SiteDeployment to Species Richness table ----
# This code is pulled directly from NABat_1_CompileVettedOutputFiles.R and will need to be edited every year
# Make sure to check the Project each SU is associated with and convert LocationNameCONUS to GRTS where necessary
# Assign correct SiteDeployment to all Monitoring Nights
SpRich <- SpRich %>% 
  mutate(SiteDeployment = 
           case_when(`Location Name` == '15834_NE1' &
                       MonitoringNight == '2023-07-06' ~ 2,
                     `Location Name` == '106997_SE1' &
                       MonitoringNight == '2023-08-22' ~ 2,
                     `Location Name` == '110715_NW1' &
                       MonitoringNight %in% c('2023-07-11','2023-07-12') ~ 2,
                     `Location Name` == '110715_SW1' &
                       MonitoringNight %in% c('2023-07-11','2023-07-12') ~ 2,
                     `Location Name` == '113007_SW1' &
                       MonitoringNight == '2023-08-03' ~ 2,
                     `Location Name` == '131055_SW1' &
                       MonitoringNight == '2023-08-31' ~ 2,
                     TRUE ~ 1))

## Join metadata to species richness table ----
dat <- SpRich %>% 
  left_join(meta %>% 
              select(-UnusualOccurrences),
            by = c('GRTS Cell Id',
                   'Location Name',
                   'SiteDeployment'))



write_csv(dat, paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/SppRichnessMetadata_',project,'_',year,'.csv'), na = '')

########################################################################################


# Pull Results for USFS Region 6 ----
CellTracker<-read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_',year,'.xlsx'))
datUSFS6 <- dat %>% 
  filter(`GRTS Cell Id` %in% (CellTracker %>% 
                                filter(`FS R6` == 1) %>% 
                                pull(GRTS_ID)))

write_csv(datUSFS6, paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/SppRichnessMetadata_USFS6_',project,'_',year,'.csv'), na = '')



# Pull Results for USFS Region 1 and 4 in Idaho----
CellTracker<-read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_',year,'.xlsx'))
datUSFS4 <- dat %>% 
  filter(`GRTS Cell Id` %in% (CellTracker %>% 
                                filter(`FS R4` == 1) %>% 
                                pull(GRTS_ID)))

write_csv(datUSFS1, paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/SppRichnessMetadata_USFS04_',project,'_',year,'.csv'), na = '')



###################
# Combine All of USFS Region04 species data (ID, NV, UT, WY)

csv_file1 <- "C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichnessMetadata_USFS04_ID_2023.csv"
csv_file2 <- "C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichnessMetadata_USFS_Region04_2023.csv"

SppRich1 <- read.csv(csv_file1)
SppRich2 <- read.csv(csv_file2)

combined_data <- dplyr::bind_rows(SppRich1, SppRich2)

write.csv(combined_data,"C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichnessMetadata_AllUSFSR04_2023.csv" , row.names = FALSE)

# Create Spp Richness table for All of USFS Region 04 for reports
USFS04SppRich <- read.csv("C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichnessMetadata_AllUSFSR04_2023.csv")

Spp_data <- USFS04SppRich[,1:20]

write.csv(Spp_data,"C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichness_AllUSFSR04_2023.csv" , row.names = FALSE)




