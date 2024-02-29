### This script takes the echoclean output and copies files classified as Species or Inspect into separate folders

library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)

dir1 <- "E:/AcousticData/YRM/Oregon/Processed/OSU_DeschutesNF_110695_YR/OSU_DeschutesNF_110695_YR_20200805"
dir2 <- "E:/AcousticData/YRM/echocleanTest/Species"
dir3 <- "E:/AcousticData/YRM/echocleanTest/Inspect"


dat <- read_excel("C:/Users/emblidgp/Box/HERS_BatAcousticFiles/echocleanTest/OSU_DeschutesNF_110695_YR_20200805_CumulativeSonoBatch_v420_out.xlsx",
                  sheet = "Classify Results")

dat %>% 
  distinct(Species) %>% 
  arrange(Species)

bats <- c('EPFU',
          'LACI',
          'LANO',
          'MYCA',
          'MYEV',
          'MYLU',
          'MYYU')


species <- dat %>% 
  filter(Species %in% bats &
           Inspect == 'No')

# files identified in echoclean output
files1 <- species %>% 
  pull(Filename) %>% 
  str_extract(".+\\d{8}_\\d{6}")

# corresponding file on computer
t1 <- list.files(dir1)
files2 <- NULL
for(i in 1:length(files1)){
  t2 <- t1[str_detect(t1,files1[i])]
  files2 <- c(files2,t2)}

# Source files
files3 <- paste0(dir1,'/',files2)

# destination files
files4 <- paste0(dir2,'/',files2)

file.copy(files3,files4)


# Make sure all files copied
files5 <- list.files(dir2)
if(length(species$Path) != length(files5)){
  stop("Incomplete file copy")}



inspect <- dat %>% 
  filter(Inspect == 'Yes')

# files identified in echoclean output
files1 <- inspect %>% 
  pull(Filename) %>% 
  str_extract(".+\\d{8}_\\d{6}")

# corresponding file on computer
t1 <- list.files(dir1)
files2 <- NULL
for(i in 1:length(files1)){
  t2 <- t1[str_detect(t1,files1[i])]
  files2 <- c(files2,t2)}

# Source files
files3 <- paste0(dir1,'/',files2)

# destination files
files4 <- paste0(dir3,'/',files2)

file.copy(files3,files4)


# Make sure all files copied
files5 <- list.files(dir3)
if(length(inspect$Path) != length(files5)){
  stop("Incomplete file copy")}
