### YRM Results

library(tidyverse)
library(lubridate)

############################################
### User enter the following information ###
# Identify YRM Station of interest
Station <- 'NOCA_128305_NW1'

# Set working directory to YRM Station of interest
setwd("C:/Users/emblidgp/Box/HERS_BatAcousticFiles/YearRoundMonitoring/Washington/Processed/NPS_NOCA_128305_NW1_YR1")

# onid ID for saving to box
onid <- 'emblidgp'
############################################


# List SonoBatch files
files <- list.files(full.names = T)
files <- files[str_detect(files,"_SonoBatch_v420.txt")]

# Read in and compile SonoBatch files
acoustic <- NULL
for(i in 1:length(files)){
  tmp1 <- read_tsv(files[i], col_types = cols(.default = "c"))
  acoustic <- rbind(acoustic,tmp1)}

# Format acoustic data
acoustic <- acoustic %>% 
  mutate(Site = Station,
         Month = format(ymd_hms(str_extract(Filename,"\\d{8}_\\d{6}")) - hours(12), "%Y-%m"),
         Detected = 1)

# Species Richness table
richness <- acoustic %>% 
  filter(!is.na(SppAccp)) %>% 
  pivot_wider(id_cols = c(Site,Month), names_from = SppAccp, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>% 
  arrange(Month)

# Species Abundance table
abundance <- acoustic %>% 
  filter(!is.na(SppAccp)) %>% 
  pivot_wider(id_cols = c(Site,Month), names_from = SppAccp, values_from = Detected, values_fn = sum, values_fill = 0, names_sort = T) %>% 
  arrange(Month)

write_csv(richness,paste0("C:/Users/",
                          onid,
                          "/Box/HERS_Working/Bats/Year-round Monitoring/Results/YRM_",
                          Station,
                          "_Richness_",
                          format(Sys.time(),"%Y%m%d"),
                          ".csv"))

write_csv(abundance,paste0("C:/Users/",
                           onid,
                           "/Box/HERS_Working/Bats/Year-round Monitoring/Results/YRM_",
                          Station,
                          "_Abundance_",
                          format(Sys.time(),"%Y%m%d"),
                          ".csv"))
