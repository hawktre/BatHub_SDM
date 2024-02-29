### This script is useful for comparing the contents of two tables to combine and eliminate redundant or out-of-date data

library(tidyverse)
library(readxl)
library(sqldf)

### Read in both tables
dat1 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2022.xlsx', col_types = 'text')
dat2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2022_20220525_NS.xlsx', col_types = 'text')

### Format both tables to have the same columns
names(dat1)
dat1 <- dat1 %>% 
  select(c(9:21,27,28,30:80))

names(dat2)
dat2 <- dat2 %>% 
  select(c(11:23,26:30,32:79))
names(dat1)[!(names(dat1) %in% names(dat2))]
names(dat2)[!(names(dat2) %in% names(dat1))]
dim(dat1)
dim(dat2)


### Subset each table to be only the rows that do not occur in the other
dat1.2 <- sqldf('SELECT * FROM dat1 EXCEPT SELECT * FROM dat2') %>% 
  arrange(CONUS_10KM)
dat2.2 <- sqldf('SELECT * FROM dat2 EXCEPT SELECT * FROM dat1') %>% 
  arrange(CONUS_10KM)

### Arrange columns of dat2.2 to match dat1.2
dat2.2 <- dat2.2[names(dat1.2)]

dim(dat1.2)
dim(dat2.2)
names(dat1.2)
names(dat2.2)


### Loop through columns to find those that do not match for the two dataframes
tst <- data.frame(col = names(dat1.2),same = NA)
for(i in 1:length(names(dat1.2))){
  tst$same[i] <- all(dat1.2[,i] == dat2.2[,i] | 
                       (is.na(dat1.2[,i]) &
                          is.na(dat2.2[,i])))
}

tst
### columns with discrepancy: 4,5,9,10,11,12,13,14,20

### check each individually to find Cells with discrepancies
i=20
names(dat1.2[i])


dat1.2$CONUS_10KM[is.na(dat1.2[,i] == dat2.2[,i] | 
                          dat1.2[,i] != dat2.2[,i] |
                          (is.na(dat1.2[,i]) &
                             is.na(dat2.2[,i])))]

#dat1.2[,i] == dat2.2[,i] | 
#  (is.na(dat1.2[,i]) &
#     is.na(dat2.2[,i]))
#all(dat1.2[,i] == dat2.2[,i] | 
#      (is.na(dat1.2[,i]) &
#         is.na(dat2.2[,i])))


dat1.2 %>% 
  left_join(dat2.2, by = 'CONUS_10KM') %>% 
  filter(CONUS_10KM %in% dat1.2$CONUS_10KM[is.na(dat1.2[,i] == dat2.2[,i] | 
                                                   dat1.2[,i] != dat2.2[,i] |
                                                   (is.na(dat1.2[,i]) &
                                                      is.na(dat2.2[,i])))]) %>% 
  select(CONUS_10KM,paste0(names(dat1.2[i]),'.x'),paste0(names(dat1.2[i]),'.y'))
  

