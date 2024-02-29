library(dplyr)
library(lubridate)
cells <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ColvilleNF/ColvilleNF_GridCells.csv')
head(cells)
cells <- cells %>% 
  select(CONUS_10KM,GRTS_ID)

### 2018 data, query Access database
### Instead of querying database, can use Box\Bats\DAYMET\FINAL.xlsx
dat18 <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ColvilleNF/AnalysisOutput2018.csv')
head(dat18)
dat18 <- dat18 %>% 
  filter(Year == 2018 &
           CONUS10km %in% cells$CONUS_10KM) %>% 
  group_by(CONUS10km,Year) %>% 
  summarize(ANPA = max(ANPA),
            COTO = max(COTO),
            EPFU = max(EPFU),
            EUMA = max(EUMA),
            LACI = max(LACI),
            LANO = max(LANO),
            MYCA = max(MYCA),
            MYCI = max(MYCI),
            MYEV = max(MYEV),
            MYLU = max(MYLU),
            MYTH = max(MYTH),
            MYVO = max(MYVO),
            MYYU = max(MYYU),
            PAHE = max(PAHE),
            TABR = max(TABR)) %>% 
  data.frame()
dat18


library(RODBC)
db <- 'C:/Users/emblidgp/Desktop/NPSBats_BE_1_7_OR_CA_WA_Complete_20211201.accdb'
con <- odbcConnectAccess2007(db)

tblDeploymentDetection7 <- sqlFetch(con,'tblDeploymentDetection7')
head(tblDeploymentDetection7)
tblDeployment <- sqlFetch(con,'tblDeployment')
head(tblDeployment)
tblPointLocation <- sqlFetch(con,'tblPointLocation')
head(tblPointLocation)



### 2019 data
### Box/Bats/2019_Analysis/NABat/2019NWNABat_AnalysisData_20200415_RogerCheck.xlsx
dat19 <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ColvilleNF/AnalysisOutput2019.csv')
head(dat19)
dat19 <- dat19 %>% 
  filter(Year == 2019 &
           CONUS10km %in% cells$CONUS_10KM) %>% 
  group_by(CONUS10km,Year) %>% 
  summarize(ANPA = max(ANPA),
            COTO = max(COTO),
            EPFU = max(EPFU),
            EUMA = max(EUMA),
            LACI = max(LACI),
            LANO = max(LANO),
            MYCA = max(MYCA),
            MYCI = max(MYCI),
            MYEV = max(MYEV),
            MYLU = max(MYLU),
            MYTH = max(MYTH),
            MYVO = max(MYVO),
            MYYU = max(MYYU),
            PAHE = max(PAHE),
            TABR = max(TABR)) %>% 
  data.frame()
dat19


### 2020 data
### Box\Bats\2020_Analysis\AcousticOutput_OR_WA.xlsz\Sheet5
### Box\Bats\2020_Analysis\AcousticOutput_ID.xlsz\Sheet6
dat20 <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ColvilleNF/AnalysisOutput2020_OR_WA.csv')
head(dat20)

dat20 <- dat20 %>% 
  mutate(Year = 2020) %>% 
  filter(SampleUnit %in% cells$CONUS_10KM) %>% 
  group_by(SampleUnit,Year) %>% 
  summarize(ANPA = max(ANPA),
            COTO = max(COTO),
            EPFU = max(EPFU),
            EUMA = max(EUMA),
            LACI = max(LACI),
            LANO = max(LANO),
            MYCA = max(MYCA),
            MYCI = max(MYCI),
            MYEV = max(MYEV),
            MYLU = max(MYLU),
            MYTH = max(MYTH),
            MYVO = max(MYVO),
            MYYU = max(MYYU),
            PAHE = max(PAHE),
            TABR = max(TABR)) %>% 
  data.frame()
dat20

### No Idaho cells
dat20ID <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ColvilleNF/AnalysisOutput2020_ID.csv')
head(dat20ID)
dat20ID %>% 
  filter(GRTS.ID %in% cells$GRTS_ID)


### 2021 data
dat21 <- read.csv('C:/Users/emblidgp/Box/Bats/2021_Analysis/SppRichness2021.csv')
head(dat21)

dat21 <- dat21 %>% 
  mutate(Year = 2021) %>% 
  filter(CONUS10K %in% cells$CONUS_10KM) %>% 
  group_by(CONUS10K,Year) %>% 
  summarize(ANPA = max(ANPA),
            COTO = max(COTO),
            EPFU = max(EPFU),
            EUMA = max(EUMA),
            LACI = max(LACI),
            LANO = max(LANO),
            MYCA = max(MYCA),
            MYCI = max(MYCI),
            MYEV = max(MYEV),
            MYLU = max(MYLU),
            MYTH = max(MYTH),
            MYVO = max(MYVO),
            MYYU = max(MYYU),
            PAHE = max(PAHE),
            TABR = max(TABR)) %>% 
  data.frame()
dat21

names(dat18)[1] <- 'SampleUnit_CONUS'
names(dat19) <- names(dat18)
names(dat20) <- names(dat18)
names(dat21) <- names(dat18)

out <- dat18 %>% 
  rbind(dat19,dat20,dat21) %>% 
  arrange(SampleUnit_CONUS,Year)

write.csv(out, 'C:/Users/emblidgp/Desktop/DataRequests/ColvilleNF/ColvilleNF_SpeciesOccurrence_2018_2021.csv', row.names = F)
