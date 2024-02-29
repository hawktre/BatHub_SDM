### This script provides tools for editing file timestamp and Station Location Name in SonoBat and Vetting output files.
### If you run this script following file attributing, and before auto-classifying, you will only need to edit Processed File Name
### See FileEditsPostProcessing_FileTimestamp.R for tools to edit Timestamp in attributed files that have not been auto-classified yet
### Files to edit:
###   _CumulativeSonoBatch_v420.txt
###   _CumulativeSonoBatch_v420-NightlySummary.txt
###   _Quad_SonoBatch_v420.txt
###   _Database.txt

# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               sjmisc)

setwd('E:/NABat/Washington/Processed/2022/115351_Incomplete')

### Identify columns containing a string
### This was used to identify the columns to edit
from <- '2021'
out <- data.frame(col = character(),
                  fix = logical())
for(i in 1:length(names(a))){
  out <- out %>% 
    add_row(col = names(a)[i], fix = str_contains(a[,i], from))
  }
out %>% 
  filter(fix == TRUE)



### Zero-Crossing Data ###
setwd('E:/NABat/AcousticData/Idaho/Processed/2021/KaleidoscopeOutput/545_ZC')
list.files()
k=7
list.files()[k]
a <- read.csv(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.csv(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)
head(a)

a <- a %>% 
  mutate(FOLDER = str_replace_all(FOLDER, 'SE2', 'SE1'))

         #         INPATHMD5 = str_replace_all(INPATHMD5, 'SE3', 'SE2'))

### Change column names back to the originals
names(a) <- b

### Save file with corrected Quad Numbers
new.name <- list.files()[k] %>% 
  str_remove('.csv') %>% 
  paste0('_pgeCorrected.csv')

#write.csv(a, file = new.name, row.names = FALSE, quote = FALSE)



########################################
### For Site_SonoBatch_v420.txt ###
########################################
setwd('E:/NABat/AcousticData/Idaho/Processed/2021/126074')
setwd('D:/NABat/Acoustic Data/Washington/Processed/2021/priority/123670')
setwd('D:/NABat/Acoustic Data/Oregon/Processed/2021/priority/101421')
list.files()
k = 7
list.files()[k]
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)
head(a)

a %>% 
  distinct(Path, Filename, ParentDir)

from <- '3914'
to <- '126074'

a <- a %>% 
  mutate(Path = str_replace_all(Path, from, to),
         Filename = str_replace_all(Filename, from, to),
         ParentDir = str_replace_all(ParentDir, from, to))


############################
### Adjusting timestamps ###
############################
### What columns to edit
from <- '20000101'
out <- data.frame(col = character(),
                  fix = logical())
for(i in 1:length(names(a))){
  out <- out %>% 
    add_row(col = names(a)[i], fix = str_contains(a[,i], from))
}
out %>% 
  filter(fix == TRUE)

### Path and Filename
tmp1 <- str_extract(a$Path, '(?<=-).{15}')
tmp2 <- force_tz(ymd_hms(tmp1), 'America/Los_Angeles')
tmp3 <- tmp2 + years(21) + months(6) + days(20) + hours(19) + minutes(30)
#tmp3 <- tmp2 + years(21) + months(3) + days(22) + hours(5) + minutes(35)
tmp4 <- format(tmp3, '%Y%m%d_%H%M%S')

a <- a %>% 
  mutate(Path = str_replace(.$Path, tmp1, tmp4),
         Filename = str_replace(.$Filename, tmp1, tmp4))




### Change column names back to the originals
names(a) <- b

### Save file with corrected Quad Numbers
new.name <- list.files()[k] %>% 
  str_remove('.txt') %>% 
  paste0('_pgeCorrected.txt')

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)





########################################
### For CumulativeSonoBatch_v420.txt ###
########################################
list.files()
k = 5
list.files()[k]
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)
head(a)

### Simple change
from <- '3914'
to <- '126074'

a <- a %>% 
  mutate(Path = str_replace_all(Path, from, to),
         Filename = str_replace_all(Filename, from, to),
         ParentDir = str_replace_all(ParentDir, from, to))

############################
### Adjusting Timestamps ###
############################
a %>% 
  mutate(tmpNE1 = str_extract(a$Path, '(?<=-).{15}')) %>% 
  filter(str_detect(Path, 'NE3'))

### Path and Filename
a <- a %>% 
  mutate(tmpNE1 = str_extract(a$Path, '(?<=-).{15}') %>% 
           ymd_hms() %>%
           force_tz('America/Los_Angeles') + years(21) + months(5) + days(7) + hours(19) + minutes(30),
         tmpNE3 = str_extract(a$Path, '(?<=-).{15}') %>% 
           ymd_hms() %>%
           force_tz('America/Los_Angeles') + years(21) + months(3) + days(22) + hours(5) + minutes(35),
         Path = 
           case_when(str_detect(Path, 'NE1') ~ 
                       str_replace(Path,
                         str_extract(Path, '(?<=-).{15}'),
                         format(tmpNE1, '%Y%m%d_%H%M%S')),
                     str_detect(Path, 'NE3') ~ 
                       str_replace(Path,
                                   str_extract(Path, '(?<=-).{15}'),
                                   format(tmpNE3, '%Y%m%d_%H%M%S')),
                     TRUE ~ Path),
         Filename = 
           case_when(str_detect(Filename, 'NE1') ~ 
                       str_replace(Filename,
                                   str_extract(Filename, '(?<=-).{15}'),
                                   format(tmpNE1, '%Y%m%d_%H%M%S')),
                     str_detect(Filename, 'NE3') ~ 
                       str_replace(Filename,
                                   str_extract(Filename, '(?<=-).{15}'),
                                   format(tmpNE3, '%Y%m%d_%H%M%S')),
                     TRUE ~ Filename)) %>%
  select(-tmpNE1, -tmpNE3)



### Complex Site swap
tmp1 <- 'SE1'      # First quad to switch
tmp2 <- 'SW1'      # Second quad to switch

a <- a %>% 
  mutate(Path = str_replace_all(Path, tmp2, 'NESW999'),
         Filename = str_replace_all(Filename, tmp2, 'NESW999'),
         ParentDir = str_replace_all(ParentDir, tmp2, 'NESW999'))
a <- a %>% 
  mutate(Path = str_replace_all(Path, tmp1, tmp2),
         Filename = str_replace_all(Filename, tmp1, tmp2),
         ParentDir = str_replace_all(ParentDir, tmp1, tmp2))
a <- a %>% 
  mutate(Path = str_replace_all(Path, 'NESW999', tmp1),
         Filename = str_replace_all(Filename, 'NESW999', tmp1),
         ParentDir = str_replace_all(ParentDir, 'NESW999', tmp1))

### Change column names back to the originals
names(a) <- b

### Save file with corrected Quad Numbers
new.name <- list.files()[k] %>% 
  str_remove('.txt') %>% 
  paste0('_pgeCorrected.txt')

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)



#######################################################
### For CumulativeSonoBatch_v420-NightlySummary.txt ###
#######################################################
list.files()
k = 2
list.files()[k]
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)
head(a)


### Simple change
from <- '3914_NW13914_NE1'
to <- '126074_SW1'

a %>% 
  distinct(Site)
a <- a %>% 
  mutate(Site = str_replace_all(Site, from, to))

a %>% 
  distinct(Date)
a <- a %>% 
  mutate(Date = str_replace_all(Date, from, to))


############################
### Adjusting Timestamps ###
############################
a %>% 
  group_by(Date) %>% 
  count()
a %>% 
  filter(Site == '110232_SE1-') %>% 
  group_by(Date) %>% 
  count()
### Date
# Date is assigned to file from their correct timestamp
# 103301_NE1: All 100 files have corrected Date 20210608
# 103301_NE3: 1 file has corrected date 20210608, 4 files from 20210609
# Two possibilities
# mutate() when all dates for a Site are the same
a <- a %>% 
  mutate(Date = 
           case_when(str_detect(Site, 'NE2') ~ '20210607',
                     TRUE ~ Date))

# When new Date spans multiple days, look at updated Processed files to see how may are from each Date, then assign using base R
tmp1 <- c(rep('20210721',10), rep('20210722',9))
a$Date[str_detect(a$Site, 'SE1') & str_detect(a$Date, '\\d+')] <- tmp1




### Complex Site swap
tmp1 <- 'SE1'      # First quad to switch
tmp2 <- 'SW1'      # Second quad to switch

a <- a %>% 
  mutate(Site = str_replace_all(Site, tmp2, 'NESW999'))
a <- a %>% 
  mutate(Site = str_replace_all(Site, tmp1, tmp2))
a <- a %>% 
  mutate(Site = str_replace_all(Site, 'NESW999', tmp1))

### Change column names back to the originals
names(a) <- b

### Save file with corrected Quad Numbers
new.name <- list.files()[k] %>% 
  str_remove('.txt') %>% 
  paste0('_pgeCorrected.txt')

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)





#############################
### For Site_Database.txt ###
#############################
setwd('D:/NABat/Acoustic Data/Oregon/Processed/2021/priority')
setwd('D:/NABat/Acoustic Data/Washington/Processed/2021/priority')
setwd('E:/NABat/AcousticData/Idaho/Processed/2021')
setwd('E:/NABat/AcousticData/Idaho/Processed/2021/100103')

list.files()
k = 2
list.files()[k]
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)
head(a)
dim(a)

out <- data.frame(col = character(),
                  fix = logical())
for(i in 1:length(names(a))){
  out <- out %>% 
    add_row(col = names(a)[i], fix = str_contains(a[,i], 'SW1'))
}
out %>% 
  filter(fix == TRUE)

### Simple filename edit
from <- 'SW1'
to <- 'SW2'

a %>% 
  filter(str_detect(.$Path, from))
a %>% 
  filter(str_detect(.$Filename, from))
a %>% 
  filter(str_detect(.$ParentDir, from))

a <- a %>% 
  mutate(Path = str_replace_all(Path, from, to),
         Filename = str_replace_all(Filename, from, to),
         ParentDir = str_replace_all(ParentDir, from, to))

############################
### Adjusting Timestamps ###
############################
### MonitoringNight, Path, and Filename
a %>% 
  group_by(MonitoringNight) %>% 
  count()
a <- a %>% 
  mutate(MonitoringNight = 
           case_when(MonitoringNight == '1999-12-31' ~ '2021-07-21',
                     MonitoringNight == '2000-01-01' ~ '2021-07-22',
                     TRUE ~ MonitoringNight))


a <- a %>% 
  mutate(tmpSE1 = str_extract(a$Path, '(?<=-).{15}') %>% 
           ymd_hms() %>%
           force_tz('America/Los_Angeles') + years(21) + months(6) + days(20) + hours(19) + minutes(30),
#         tmpNE3 = str_extract(a$Path, '(?<=-).{15}') %>% 
#           ymd_hms() %>%
#           force_tz('America/Los_Angeles') + years(21) + months(3) + days(22) + hours(5) + minutes(35),
         Path = 
           case_when(str_detect(Path, 'SE1') ~ 
                       str_replace(Path,
                                   str_extract(Path, '(?<=-).{15}'),
                                   format(tmpSE1, '%Y%m%d_%H%M%S')),
#                     str_detect(Path, 'NE3') ~ 
#                       str_replace(Path,
#                                   str_extract(Path, '(?<=-).{15}'),
#                                   format(tmpNE3, '%Y%m%d_%H%M%S')),
                     TRUE ~ Path),
         Filename = 
           case_when(str_detect(Filename, 'SE1') ~ 
                       str_replace(Filename,
                                   str_extract(Filename, '(?<=-).{15}'),
                                   format(tmpSE1, '%Y%m%d_%H%M%S')),
#                     str_detect(Filename, 'NE3') ~ 
#                       str_replace(Filename,
#                                   str_extract(Filename, '(?<=-).{15}'),
#                                   format(tmpNE3, '%Y%m%d_%H%M%S')),
                     TRUE ~ Filename)) %>%
  select(-tmpSE1)


### Simple Date edit
a %>% 
  distinct(MonitoringNight)

from <- '2020'
to <- '2021'

a <- a %>% 
  mutate(MonitoringNight = str_replace_all(MonitoringNight, from, to))




### Complex Site swap
a <- a %>% 
  mutate(Path = str_replace_all(Path, tmp2, 'NESW999'),
         Filename = str_replace_all(Filename, tmp2, 'NESW999'),
         ParentDir = str_replace_all(ParentDir, tmp2, 'NESW999'))
a <- a %>% 
  mutate(Path = str_replace_all(Path, tmp1, tmp2),
         Filename = str_replace_all(Filename, tmp1, tmp2),
         ParentDir = str_replace_all(ParentDir, tmp1, tmp2))
a <- a %>% 
  mutate(Path = str_replace_all(Path, 'NESW999', tmp1),
         Filename = str_replace_all(Filename, 'NESW999', tmp1),
         ParentDir = str_replace_all(ParentDir, 'NESW999', tmp1))

### Change column names back to the originals
names(a) <- b

### Save file with corrected Quad Numbers
new.name <- list.files()[k] %>% 
  str_remove('.txt') %>% 
  paste0('_pgeCorrected.txt')

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)





#####################################
### NABat Bulk Upload corrections ###
#####################################
setwd('C:/Users/emblidgp/Desktop')
list.files()
k=16
list.files()[k]

a <- read.csv(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.csv(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)
head(a)

out <- data.frame(col = character(),
                  fix = logical())
for(i in 1:length(names(a))){
  out <- out %>% 
    add_row(col = names(a)[i], fix = str_contains(a[,i], '101421_SE2'))
}
out %>% 
  filter(fix == TRUE)


### Simple Edit
from <- '101421_SE2'
to <- '101421_SE1'

a <- a %>% 
  mutate(Location.Name = str_replace_all(Location.Name, from, to),
         Audio.Recording.Name = str_replace_all(Audio.Recording.Name, from, to))
a %>% 
  filter(str_detect(Location.Name, to))
a %>% 
  filter(str_detect(Location.Name, from))



### Complex Site swap
tmp1 <- '119494_NW1'
tmp2 <- '119494_NW2'

a <- a %>% 
  mutate(Location.Name = str_replace_all(Location.Name, tmp2, 'NESW999'),
         Audio.Recording.Name = str_replace_all(Audio.Recording.Name, tmp2, 'NESW999'))
a <- a %>% 
  mutate(Location.Name = str_replace_all(Location.Name, tmp1, tmp2),
         Audio.Recording.Name = str_replace_all(Audio.Recording.Name, tmp1, tmp2))
a <- a %>% 
  mutate(Location.Name = str_replace_all(Location.Name, 'NESW999', tmp1),
         Audio.Recording.Name = str_replace_all(Audio.Recording.Name, 'NESW999', tmp1))

### Change column names back to the originals
names(a) <- b

### Save file with corrected Quad Numbers
new.name <- list.files()[k] %>% 
  str_remove('.csv') %>% 
  paste0('_pgeCorrected.csv')

#write.csv(a, file = new.name, row.names = FALSE, quote = FALSE)





###################################################
### Change auto-classified files in Quad folder ###
###################################################
from <- 'SE2'
to <- 'SE1'
setwd('D:/NABat/Acoustic Data/Oregon/Processed/2021/priority/101421/101421_SE1')
setwd('D:/NABat/Acoustic Data/Washington/Processed/2021/priority/123670/123670_SE1')
setwd('E:/NABat/AcousticData/Idaho/Processed/2021/126074/126074_SW1')

files <- list.files()
files
files2 <- files %>% 
  str_replace(from, to)
files2
#file.rename(files, files2)

### Remove duplicate files
files2 <- files[str_detect(files, '\\[1\\]')]
#file.remove(files2)
getwd()

### Adjusting timestamps
setwd('D:/NABat/Acoustic Data/Oregon/Processed/2021/110232/110232_SE1')
files <- list.files()
files
tmp1 <- str_extract(files, '(?<=-).{15}')
tmp2 <- force_tz(ymd_hms(tmp1), 'America/Los_Angeles')
tmp3 <- tmp2 + years(21) + months(6) + days(20) + hours(19) + minutes(30)
#tmp3 <- tmp2 + years(21) + months(3) + days(22) + hours(5) + minutes(35)
tmp4 <- format(tmp3, '%Y%m%d_%H%M%S')
files2 <- files %>% 
  str_replace(tmp1, tmp4)
files2
#file.rename(files, files2)




###################################################
### Detectors that are recording the wrong year ###
###################################################

### Rename processed files
setwd('E:/NABat/Idaho/Processed/2022/4938/4938_NE2')
files <- list.files()
files2 <- str_replace(files, '(?<=-)2021', '2022')
file.rename(files,files2)


### Quad SonoBatch file
setwd('E:/NABat/Idaho/Processed/2022/4938')
list.files()
k=5
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)

from = '(?<=-)2021'
to = '2022'

### Check which columns from occurs in
data.frame(col = names(a),
           check = a %>% str_detect(from)) %>% 
  filter(check == TRUE)


### Find and Replace
a <- a %>% 
  mutate(Path = str_replace(Path, from, to),
         Filename = str_replace(Filename, from, to))

### Save file with corrected Quad Numbers
new.name <- list.files()[k]

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)



### For CumulativeSonoBatch_v420.txt
list.files()
k = 3
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)

from = '(?<=-)2021'
to = '2022'

data.frame(col = names(a),
           check = a %>% str_detect(from)) %>% 
  filter(check == TRUE)

a <- a %>% 
  mutate(Path = str_replace(Path, from, to),
         Filename = str_replace(Filename, from, to))

new.name <- list.files()[k]

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)



### Nightly Summary
list.files()
k = 2
list.files()[k]
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE, check.names = FALSE)
b <- names(a)
a <- read.delim(list.files()[k], colClasses = c('character'), blank.lines.skip = FALSE)

from <- '2021'
to <- '2022'

data.frame(col = names(a),
           check = a %>% str_detect(from)) %>% 
  filter(check == TRUE)

a %>% 
  distinct(Date)
a <- a %>% 
  mutate(Date = str_replace_all(Date, from, to))

names(a) <- b

new.name <- list.files()[k]

#write.table(a, file = new.name, sep = "\t", row.names = FALSE, quote = FALSE)


### Create ReadME file
### Transfer edited files to Box
