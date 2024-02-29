rm(list = ls())
library(tidyverse)
install.packages(c('devtools','dplyr','flextable','ggplot2','htmltools','htmlwidgets',
                   'httr','jsonlite','leaflet','lubridate','magrittr','maps','maptools','mapview','officer',
                   'plotly','plyr','raster','rgdal','rmarkdown','sp','xml2', 'stringr'))


install.packages('vctrs')
library(vctrs)
devtools::install_git("https://code.usgs.gov/fort/nabat/nabatr.git", force = T)

#devtools::install_github('usgs/nabatr', build_vignettes = TRUE, upgrade = 'never', force = TRUE)

library(nabatr)
library(sp)

browseVignettes('nabatr')

# Get GRTS Data
SampleUnitsUT <- get_grts_data('conus',query = "state_n_1='Utah'")
names(SampleUnitsUT)
spplot(SampleUnitsUT,zcol='own_USFS')

SampleUnitsUT_priority <- get_grts_data('conus', query="state_n_1='Utah'", only_priority = T)
spplot(SampleUnitsUT_priority,zcol='own_USFS')


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
states <- map_data("state")
counties <- map_data("county")
UT_state <- subset(states, region=="utah")
UT_counties <- subset(counties, region=="utah")

base <- ggplot(data = UT_state,
               mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = 'black', fill = 'grey')

base +
  geom_polygon(data = spTransform(SampleUnitsUT_priority,CRS("+proj=longlat +datum=WGS84")), color = 'grey30')


# Use grts to confirm locations
library(readxl)
library(sf)
dat <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx")
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)
dat <- dat %>% 
  left_join(CellList,by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(GRTS =
           case_when(State == "ID" ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         CONUS = 
           case_when(State == "ID" ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`)) %>% 
  select(-c(GRTS_ID,CONUS_10KM))

dat.sf <- st_as_sf(dat %>% 
                     filter(!is.na(x)),
                   coords = c('x','y'))
st_crs(dat.sf) <- 4269


SUs <- nabatr::get_grts_data('conus',query = "state_n_1 IN ('Oregon','Washington','Idaho')")

SUs <- st_as_sf(SUs)
SUs <- st_transform(SUs, 4269)

dat.sf <- st_join(dat.sf,SUs)
dat.sf %>% 
  filter(GRTS != GRTS_ID)


# Alternatively, pull GRTS from lat long
# Somehow this results in 829 GRTS_IDs, although only 827 coordinate pairs used
library(readxl)
library(tidyverse)
dat <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx")
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)
dat <- dat %>% 
  left_join(CellList,by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(GRTS =
           case_when(State == "ID" ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         CONUS = 
           case_when(State == "ID" ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`)) %>% 
  select(-c(GRTS_ID,CONUS_10KM)) %>% 
  filter(!is.na(x))

username = 'patrick.emblidge@oregonstate.edu'
token = nabatr::get_nabat_gql_token(username)

token = nabatr::get_refresh_token(token)

tmp1 <- nabatr::get_grts_from_ll(token,
                         latitude = dat$y,
                         longitude = dat$x)
class(tmp1)
str(tmp1)
names(tmp1)
tmp1$grts_cell_id


### Get Stationary Acoustic Data
library(nabatr)
username = 'patrick.emblidge@oregonstate.edu'
token = get_nabat_gql_token(username)
token

token = get_refresh_token(token)

# Get projects lookup table
project_df <- get_projects(token)
project_df
class(project_df)
names(project_df)

### Get all stationary Acoustic Surveys within a single project set with project_id
# Refresh token
token = get_refresh_token(token)

# Identify project to pull data from
project_id <- project_df %>% 
  filter(project_name == "NABat Idaho") %>% 
  pull(project_id)

# Get survey dataframe
sa_survey_df <- get_sa_project_summary(token,
                                       project_df,
                                       project_id)
sa_survey_df


# Get survey event metadata, faster than below method of pulling WAVs
surveys <- get_sa_event_metadata(token,
                      survey_df = sa_survey_df),
                      year = 2022)
?get_sa_event_metadata

### Get all Stationary Acoustic Survey wav file data
# Select Year for project
year <- sa_proj_dates[1]

# Refresh token
token = get_refresh_token(token)

# Get stationary acoustic bulk upload format dataframe
sa_bulk_df <- get_sa_bulk_wavs(token,
                               sa_survey_df,
                               year)

# Display stationary acoustic bulk upload format dataframe
sa_bulk_df %>% 
  distinct(location_name,
           latitude,
           longitude)






### List all functions in nabatr package
ls("package:nabatr")

ls("package:nabatr")[str_detect(ls("package:nabatr"),"grid")]

### Get survey data
# get_project_surveys() is DEPRICATED, see get_acoustic_project_summary()
?get_project_surveys
surveys <- get_project_surveys(token,
                                        project_df,
                                        project_id)
surveys


??get_acoustic_project_summary
nabatr::get_acoustic_project_summary()




### Build Stationary Acoustic Report ####
### Get a token for NABat Database
# Enter your NABat username here
username = 'NABat_Username'
token = get_nabat_gql_token(username)
token

### Get Project dataframe which contains all of your NABat projects in the NABat Database
# Refresh token
token = get_refresh_token(token)
# Get your projects lookup table
project_df = get_projects(token)
# Display your projects lookup table
project_df


### Get all Stationary Acoustic Surveys within a single Project set with project_id
# Refresh token
token = get_refresh_token(token)
# Fill in project id using the project_df lookup table
project_df %>% filter(str_detect(project_name,'NW'))
project_id = 326 # Set equal to one of your project ids

# Get survey dataframe 
sa_survey_df = get_sa_project_summary(token, 
                                      project_df, 
                                      project_id)

?get_sa_event_metadata
get_sa_event_metadata(token,
                      survey_df = sa_survey_df)

dim(sa_survey_df)
sa_proj_dates = unique(sa_survey_df$year)
# Display survey dataframe
sa_survey_df


### Get all Stationary Acoustic Survey wav file data.  Format: Stationary Acoustic Bulk Upload Template
# Select Year for project (defaults to 1st available year)
year = sa_proj_dates[1]

# Refresh token
token = get_refresh_token(token)
# Get stationary acoustic bulk upload format dataframe
sa_bulk_df = get_sa_bulk_wavs(token, 
                              sa_survey_df,
                              year)
# Display stationary acoustic bulk upload format dataframe
sa_bulk_df
```


### Get Stationary Acoustic dataframe broken up into nightly observed data and get species_df
```
token = get_refresh_token(token)
species_df = get_species(token)
# Get Acoustic stationary acoustic bulk dataframe
nightly_observed_list = get_observed_nights(sa_bulk_df)
```


### Build a Stationary Acoustic Report
```
# Edit these two variables below to your local system (file_name, out_dir)
file_name = 'name_of_sa_report.docx'  
out_dir   = '/directory/to/download/report' # Make sure this ends without a '/'

sa_doc = build_sa_doc(out_dir = out_dir,
                      project_df = project_df,
                      project_id = project_id,
                      sa_bulk_df = sa_bulk_df,
                      sa_survey_df = sa_survey_df,
                      species_df = species_df,
                      selected_year = year,
                      nightly_observed_list = nightly_observed_list,
                      range_maps = TRUE)

# Save out your report
print(sa_doc, target = paste0(out_dir, '/', file_name))
```