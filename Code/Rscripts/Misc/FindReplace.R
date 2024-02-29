############################################
### Correct Quad numbers post-processing ###
############################################
library(tidyverse)
library(readxl)
library(writexl)


setwd("C:/Users/emblidgp/Downloads")
list.files()
k=114
a <- read_tsv(list.files()[k],
              col_types = cols(.default = "c"),
              na = character())
a <- read_csv(list.files()[k],
              col_types = cols(.default = "c"),
              na = character())

head(a)

#a <- a %>% 
#  select(-c('MYLU','MYCA'))

a %>% 
  filter(`Species Manual ID` == 'Lano') %>% 
  distinct(ParentDir)

a %>% 
  filter(`Species Manual ID` == 'Myca') %>% 
  distinct(ParentDir)



### Correct all misassigned quad numbers
from = 'Lano'
to = 'POS_LANO'

### Check which columns from occurs in
data.frame(col = names(a),
           check = a %>% str_detect(from)) %>% 
  filter(check == TRUE)


### Check which columns to occurs in
data.frame(col = names(a),
           check = a %>% str_detect(to)) %>% 
  filter(check == TRUE)


### Find and Replace
a <- a %>% 
  mutate(across(`Species Manual ID`, str_replace_all, pattern = from, replacement = to))

a <- a %>% 
  mutate(`Species Manual ID` =
           case_when(ParentDir == 'PARA_62120_NW2_20210528' &
                       `Species Manual ID` == 'Lano' ~ 'POS_LANO',
                     TRUE ~ `Species Manual ID`))


### Check columns of interest
a %>% 
  distinct(`Species Manual ID`)


write_tsv(a, list.files()[k])
write_csv(a, list.files()[k])
