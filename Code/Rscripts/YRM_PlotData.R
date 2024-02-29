library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)

tst1 <- data.frame(week = 1:52,
                   b1 = sample(c(0,1),52,replace = T))
tst2 <- data.frame(week = 1:52,
                   b1 = sample(c(0,1),52,replace = T),
                   b2 = sample(c(0,1),52,replace = T),
                   b3 = sample(c(0,1),52,replace = T),
                   b4 = sample(c(0,1),52,replace = T),
                   b5 = sample(c(0,1),52,replace = T),
                   b6 = sample(c(0,1),52,replace = T),
                   b7 = sample(c(0,1),52,replace = T),
                   b8 = sample(c(0,1),52,replace = T),
                   b9 = sample(c(0,1),52,replace = T),
                   b10 = sample(c(0,1),52,replace = T),
                   b11 = sample(c(0,1),52,replace = T),
                   b12 = sample(c(0,1),52,replace = T),
                   b13 = sample(c(0,1),52,replace = T),
                   b14 = sample(c(0,1),52,replace = T),
                   b15 = sample(c(0,1),52,replace = T))
ggplot(tst1, aes(x=week,y=b1))+
  geom_bar(width=1,stat = 'identity')+
  coord_polar('x', start=0)

ggplot()+
  geom_point(data=tst2, aes(x=week,y=b1,color=1))+
  geom_point(data=tst2, aes(x=week,y=b2*2,color=2))+
  geom_point(data=tst2, aes(x=week,y=b3*3,color=3))+
  geom_point(data=tst2, aes(x=week,y=b4*4,color=4))+
  geom_point(data=tst2, aes(x=week,y=b5*5,color=5))+
  geom_point(data=tst2, aes(x=week,y=b6*6,color=6))+
  geom_point(data=tst2, aes(x=week,y=b7*7,color=7))+
  geom_point(data=tst2, aes(x=week,y=b8*8,color=8))+
  geom_point(data=tst2, aes(x=week,y=b9*9,color=9))+
  geom_point(data=tst2, aes(x=week,y=b10*10,color=10))+
  geom_point(data=tst2, aes(x=week,y=b11*11,color=11))+
  geom_point(data=tst2, aes(x=week,y=b12*12,color=12))+
  geom_point(data=tst2, aes(x=week,y=b13*13,color=13))+
  geom_point(data=tst2, aes(x=week,y=b14*14,color=14))+
  geom_point(data=tst2, aes(x=week,y=b15*15,color=15))






dat <- read.csv('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_output.csv')
bats <- read.csv('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Year Round Monitoring/Oregon/Processed/BLM_SppRichness.csv',colClasses = 'character')
head(dat)
head(bats)

### Fix bats Weeks, adding leading 0 to single digit EpiWeeks
bats <- bats %>% 
  mutate(Week = 
           case_when(str_detect(Week,'_\\d$') ~ paste0(str_extract(Week,'^\\d{4}'),'_0',str_extract(Week,'\\d$')),
                     TRUE ~ Week))

### Same for dat
dat <- dat %>% 
  mutate(ParentDir = 
           case_when(str_detect(ParentDir,'_\\d$') ~ paste0(str_extract(ParentDir,'^\\d{4}'),'_0',str_extract(ParentDir,'\\d$')),
                     TRUE ~ ParentDir))


### Stations
dat %>% 
  distinct(NextDirUp)

dat <- dat %>% 
  filter(NextDirUp != '')


#site <- 'Klamath'

#dat %>% 
#  filter(str_detect(NextDirUp,site)) %>% 
#  group_by(ParentDir) %>% 
#  summarize(nights = length(unique(MonitoringNight))) %>% 
#  arrange(nights)


### Species Richness table did not include weeks without any bats detected, add them from dat
bats <- bats %>% 
  add_row(dat %>% 
            filter(ParentDir != '') %>% 
            anti_join(bats,by = c('NextDirUp' = 'Station', 'ParentDir' = 'Week')) %>% 
            distinct(NextDirUp,ParentDir) %>% 
            rename('Station' = 'NextDirUp',
                   'Week' = 'ParentDir') %>% 
            mutate(ANPA = '0',
                   COTO = '0',
                   EPFU = '0',
                   EUMA = '0',
                   LACI = '0',
                   LANO = '0',
                   MYCA = '0',
                   MYCI = '0',
                   MYEV = '0',
                   MYLU = '0',
                   MYTH = '0',
                   MYVO = '0',
                   MYYU = '0',
                   PAHE = '0',
                   TABR = '0'))







### Check that there are the same number of rows in dat and bats
dat %>% 
  distinct(NextDirUp,ParentDir)
bats %>% 
  distinct(Station,Week)



### Add a count of Monitoring nights with at least 1 call file for each week
tmp2 <- dat %>% 
  group_by(NextDirUp, ParentDir) %>% 
  summarize(nights = length(unique(MonitoringNight))) %>% 
  arrange(NextDirUp,ParentDir)

bats <- bats %>% 
  left_join(tmp2, by = c('Station' = 'NextDirUp', 'Week' = 'ParentDir'))

bats %>% 
  filter(is.na(nights))



### fill in data with missing epiweeks
### Create a data.frame of the sequence of dates the station has been active for
tmp3 <- dat %>% 
  filter(NextDirUp != '') %>% 
  group_by(NextDirUp) %>% 
  summarize(first = ymd(min(MonitoringNight)),
            last = ymd(max(MonitoringNight)))

### Create data.frame of all weeks
tmp4 <- NULL
for(i in 1:length(tmp3$NextDirUp)){
  tst1 <- data.frame(Station = tmp3$NextDirUp[i],
                     Night = seq(tmp3$first[i],tmp3$last[i],by='days')) %>% 
    mutate(Week = epiweek(Night),
           Year = case_when(Week > 50 & month(Night) == 1 ~ year(Night) - 1,
                            Week < 5 & month(Night) == 12 ~ year(Night) + 1,
                            TRUE ~ year(Night)),
           Epi_Week = paste(Year,str_pad(Week,2,pad = '0'),sep='_')) %>% 
    distinct(Station,Epi_Week) %>% 
    rename('Week' = 'Epi_Week')
  tmp4 <- tmp4 %>% 
    rbind(tst1)
}

### Limit to only weeks not accounted for in bats
dim(bats)
dim(tmp4)

tmp4 <- tmp4 %>% 
  anti_join(bats) %>% 
  mutate(ANPA = NA_character_,
         COTO = NA_character_,
         EPFU = NA_character_,
         EUMA = NA_character_,
         LACI = NA_character_,
         LANO = NA_character_,
         MYCA = NA_character_,
         MYCI = NA_character_,
         MYEV = NA_character_,
         MYLU = NA_character_,
         MYTH = NA_character_,
         MYVO = NA_character_,
         MYYU = NA_character_,
         PAHE = NA_character_,
         TABR = NA_character_,
         nights = 0)


#tmp3 <- data.frame(Night = seq(ymd(min(dat$MonitoringNight[dat$NextDirUp=='BLM_CoosBay_105579_YR'])),
#                               ymd(max(dat$MonitoringNight[dat$NextDirUp=='BLM_CoosBay_105579_YR'])),
#                               by = 'days'))
#tmp3 <- tmp3 %>% 
#  mutate(Week = epiweek(Night),
#         Year = case_when(Week > 50 & month(Night) == 1 ~ year(Night) - 1,
#                          Week < 5 & month(Night) == 12 ~ year(Night) + 1,
#                          TRUE ~ year(Night)),
#         Epi_Week = paste(Year,str_pad(Week,2,pad = '0'),sep='_'))

#all_weeks <- unique(tmp3$Epi_Week)

#all_weeks


### Add empty rows from detector failures
# Create empty data.frame using bats as template
#tmp4 <- data.frame(Station = 'BLM_CoosBay_105579_YR',
#         Week = all_weeks[!(all_weeks %in% (bats %>%
#                                              filter(str_detect(Station,'CoosBay')) %>%
#                                              pull(Week)))],
#         ANPA = NA_character_,
#         COTO = NA_character_,
#         EPFU = NA_character_,
#         EUMA = NA_character_,
#         LACI = NA_character_,
#         LANO = NA_character_,
#         MYCA = NA_character_,
#         MYCI = NA_character_,
#         MYEV = NA_character_,
#         MYLU = NA_character_,
#         MYTH = NA_character_,
#         MYVO = NA_character_,
#         MYYU = NA_character_,
#         PAHE = NA_character_,
#         TABR = NA_character_,
#         nights = 0)

bats <- bats %>% 
  add_row(tmp4)


### Add Date as X-axis, Wednesday of the EpiWeek
### Find first epiweek
dat %>% 
  filter(NextDirUp != '') %>% 
  summarize(min(ParentDir))
### Look up the MonitoringNights of the first epiweek, and compare with calendar to confirm Wednesday's date
dat %>% 
  filter(ParentDir=='2020_23') %>% 
  distinct(MonitoringNight)

tmp5 <- bats %>% 
  distinct(Week) %>% 
  arrange(Week)
  
tmp5 <- tmp5 %>% 
  mutate(
    Date = ymd('2020-06-03') + days(7*(row_number()-1)))

bats <- bats %>% 
  left_join(tmp5)

bats %>% 
  distinct(TABR)

bats2 %>% 
  distinct(Presence)

bats2 <- bats2 %>% 
  mutate(Presence = case_when(is.na(Presence) ~ '3',
                              TRUE ~ Presence),
         Presence = factor(Presence, levels = c(1,0,3), labels = c('Confirmed','Not Detected', 'Detector Failure')))

### Make a pretty plot
### Format
bats2 <- bats %>% 
  pivot_longer(cols = ANPA:TABR, names_to = 'Species', values_to = 'Presence')
# %>% filter(!is.na(Presence))

ggplot()+
  geom_vline(data = (bats %>% 
                       filter(nights > 0 &
                                nights < 7)),
             aes(xintercept = Date),
             col='grey',
             size=2.5) +
  geom_vline(data = (bats %>% 
                       filter(nights == 0)),
             aes(xintercept = Date),
             col = 'firebrick',
             size=2.5) +
  geom_point(data = bats2, aes(x=Date, y=Species, size = Presence)) +
  labs(title = 'BLM Year Round Monitoring Stations',
       subtitle = 'Weekly species verification')+
  #  scale_size_manual(values = c(2,5)) +
  #  scale_shape_manual(values = c(16,18)) +
  theme_bw() +
  facet_wrap(~ Station)




### Subset and Plot individually
bats %>% 
  distinct(Station)
site = 'Vale'

bats3 <- bats %>% 
  filter(str_detect(Station,site))
bats4 <- bats2 %>% 
  filter(str_detect(Station,site))

#tiff(paste0('C:/Users/emblidgp/Box/HERS_Working/Bats/Year-round Monitoring/Results/YRM_BLM_WeeklyDetections_',site,'.tif'),
#     res=800)
tiff(paste0('C:/Users/emblidgp/Desktop/YRM_BLM_WeeklyDetections_',site,'.tif'),
     units='in',
     height = 4,
     width = 9,
     res=300)


ggplot()+
  geom_vline(data = (bats3 %>% 
                       filter(nights > 0 &
                                nights < 7)),
             aes(xintercept = Date),
             col='grey90',
             size=3.0) +
#  geom_vline(data = (bats3 %>% 
#                       filter(nights == 0)),
#             aes(xintercept = Date),
#             col = 'firebrick',
#             size=4,
#             alpha=0.25) +
#  geom_point(data = bats4 %>% 
#               filter(Presence!='3'), aes(x=Date, y=Species, size = Presence)) +
#  geom_point(data = bats4 %>% 
#               filter(Presence=='3'), aes(x=Date, y=Species), shape = 4,col='red') +
  
  geom_point(data = bats4, aes(x=Date,y=Species,shape=Presence, col=Presence),size=1.5)+

  labs(title = paste0(site,' BLM Year Round Monitoring Station'),
       size = "Status")+

  scale_shape_manual(name="",
                     values = c(16,1,4)) +
  
  scale_x_date(date_breaks = '2 month', date_labels = '%b %Y')+
  
  scale_color_manual(name="",
                     values = c('black','black','firebrick'))+
  
  theme_bw()+
  
  theme(legend.title = element_blank(),
#        legend.title = element_text(size=14,face='bold'),
        legend.text = element_text(size = 12),
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

dev.off()



#### Let's try to incorporate weather data into plots ####
install.packages('prism')
library(prism)
install.packages('terra')

# Set the directory that the prism data will be saved to
prism_set_dl_dir("C:/Users/emblidgp/Documents/prism")




### Make plot interactive
install.packages('plotly')
library(plotly)
ggplotly(p1)

