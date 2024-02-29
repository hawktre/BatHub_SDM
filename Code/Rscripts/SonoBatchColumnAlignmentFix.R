##########################################
### SonoBat 4.4.5 tends to have poor column alignment in output files ###
##########################################
library(tidyverse)

setwd("E:/NABat/USFS_Region4/Processed/2022/450")
list.files()
k=3
a <- read_tsv(list.files()[k],
              col_types = cols(.default = "c"),
              na = character())
problems()
#parsing issue with column alignment

### Look at the Filename to know what ParentDir value should be
a %>% 
  distinct(Filename) %>% 
  pull()

a %>% 
  distinct(ParentDir) %>% 
  print(n=Inf)

### Find columns where true ParentDir occurs
pd <- '450_NE1'
out <- data.frame(col = character(),
                  index = integer(),
                  fix = logical())
for(i in 1:length(names(a))){
  out <- out %>% 
    add_row(col = names(a)[i], index = i, fix = (pd %in% (a %>% pull(i))))
}
out %>% 
  filter(fix == TRUE)


#############################
### Shift misaligned rows ###
#############################
### This is set up for pd occurring in columns 23 - 30 and ParentDir being column 27, edit if needed
### Check to make sure ParentDir is column 27
which(names(a) == 'ParentDir')

for(i in 1:length(a$Filename)){
  if(a$ParentDir[i] == pd){
  }else{
    tmp1 <- a[i,]
    tmp2 <- which(tmp1 == pd)
    if(length(tmp2) == 0){
      del <- c(del,i)}else{
    if(tmp2 == 23){
      tmp3 <- as.character(tmp1[,1:22])
      tmp4 <- as.character(tmp1[,23:30])
      tmp5 <- rep(NA,4)
      a[i,] <- as.list(c(tmp3,tmp5,tmp4))
      }
    if(tmp2 == 24){
      tmp3 <- as.character(tmp1[,1:23])
      tmp4 <- as.character(tmp1[,24:31])
      tmp5 <- rep(NA,3)
      a[i,] <- as.list(c(tmp3,tmp5,tmp4))}
    if(tmp2 == 25){
      tmp3 <- as.character(tmp1[,1:24])
      tmp4 <- as.character(tmp1[,25:32])
      tmp5 <- rep(NA,2)
      a[i,] <- as.list(c(tmp3,tmp5,tmp4))}
    if(tmp2 == 26){
      tmp3 <- as.character(tmp1[,1:25])
      tmp4 <- as.character(tmp1[,26:33])
      tmp5 <- rep(NA,1)
      a[i,] <- as.list(c(tmp3,tmp5,tmp4))}
    if(tmp2 == 28){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,28:34])
      tmp5 <- c('32')
      a[i,] <- as.list(c(tmp3,tmp4,tmp5))}
    if(tmp2 == 29){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,29:34])
      tmp5 <- c('0.20', '32')
      a[i,] <- as.list(c(tmp3,tmp4,tmp5))}
    if(tmp2 == 30){
      tmp3 <- as.character(tmp1[,1:26])
      tmp4 <- as.character(tmp1[,30:34])
      tmp5 <- c('0.80','0.20','32')
      a[i,] <- as.list(c(tmp3,tmp4,tmp5))}
}}}


### Check to make sure ParentDir aligns properly now
out <- data.frame(col = character(),
                  index = integer(),
                  fix = logical())
for(i in 1:length(names(a))){
  out <- out %>% 
    add_row(col = names(a)[i], index = i, fix = (pd %in% (a %>% pull(i))))
}
out %>% 
  filter(fix == TRUE)

a %>% 
  distinct(ParentDir)


write_tsv(a, list.files()[k])
