### tblSiteStateCounty has multiple records if a Sample Unit crosses county lines
### County data derived from C:\Users\emblidgp\Box\Bats\GIS\NABat Grid Sampling Frame\conus_mastersample_10km_attributed\conus_mastersample_10km_attributed.shp

install.packages(c('tidyverse','DBI','readxl','foreign'))

library(tidyverse)
library(DBI)
library(readxl)
library(foreign)

### RStudio uses 64 bit R by default
### MS Access and associated Drivers are 32 bit, causing mismatch errors
### Use 32 bit R for database connectivity
###   Tools > Global Options > General > R version:
### But remember to set back to 64 bit when done

### Establish database connection
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Desktop/PNW_BatHub_Database.accdb")

### Read in database tables
tblSite <- dbReadTable(con,'tblSite')
tblSiteStateCounty <- dbReadTable(con,'tblSiteStateCounty')
tluState <- dbReadTable(con,'tluState')

### Read in conus_mastersample_attributed
mastersample <- read.dbf('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/conus_mastersample_10km_attributed/conus_mastersample_10km_attributed.dbf', as.is = T)
mastersample$CONUS_10KM <- as.character(mastersample$CONUS_10KM)

### Identify all Site/State/County combinations from Sample Units currently in tblSite
tmp1 <- tblSite %>% 
  left_join(mastersample, by = c('SampleUnitID' = 'CONUS_10KM')) %>% 
  select(ID,SampleUnitID,cnty_n_1,cnty_n_2,cnty_n_3,cnty_n_4,cnty_n_5)

### Pivot longer and format to match tblSiteStateCounty
tmp2 <- tmp1 %>% 
  pivot_longer(cols = cnty_n_1:cnty_n_5) %>%
  filter(value != 'NA') %>% 
  mutate(StateName = str_extract(value, '[^_]+'),
         County = str_extract(value, '(?<=_).+')) %>% 
  left_join(tluState) %>% 
  select(ID,StateCode,County) %>% 
  rename('SiteID' = 'ID')

### Are there any mismatched SiteStateCounty records?
tblSiteStateCounty %>% 
  anti_join(tmp2)

### Site/State/County combinations not in tblSiteStateCount
tmp2 %>% 
  anti_join(tblSiteStateCounty)

### SiteStateCounty records to add from new Sample Units
tmp2 %>% 
  filter(SiteID %in% (tblSite %>% 
                        filter(!(ID %in% tblSiteStateCounty$SiteID)) %>% 
                        pull(ID)))

### SiteStateCounty records to add from existing Sample Units
tmp2 %>% 
  filter(!SiteID %in% (tblSite %>% 
                         filter(!(ID %in% tblSiteStateCounty$SiteID)) %>% 
                         pull(ID))) %>% 
  anti_join(tblSiteStateCounty)


### Take new Site/State/County records, format to match tblSiteStateCounty
sitestatecounty.new <- tmp2 %>% 
  anti_join(tblSiteStateCounty) %>% 
  mutate(CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge')
sitestatecounty.new
  

### Add new records to tblSiteStateCounty
options(odbc.batch_rows = 1)   # This option needs to be set to append multiple rows (Access-specific)
dbAppendTable(con,'tblSiteStateCounty',sitestatecounty.new)


### It's always a good idea to close database connections at the end of your session
dbDisconnect(con)
