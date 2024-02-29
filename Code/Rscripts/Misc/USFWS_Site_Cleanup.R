library(tidyverse)
library(sf)
### USFWS NABat Location Clean-Up

dat <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_DataExport_NationalWildlifeRefugesPacificNorthwestRegion1(R1)_20230303.csv')

dat <- dat %>%
  mutate(geometry = st_as_sfc(structure(geometry, class = "WKB"), EWKB = TRUE),
         Latitude = st_coordinates(geometry)[,2],
         Longitude = st_coordinates(geometry)[,1]) %>%
  st_as_sf()


# Pull all locations for a single cell
tmp1 <- dat %>% 
  filter(grts_cell_id == 12162) %>% 
  distinct(grts_cell_id,location_name,geometry)


# Calculate distance between Locations
out <- NULL
for(i in 1:length(tmp1$grts_cell_id)){
  tmp2 <- tmp1[-i,]
  tmp3 <- data.frame(grts = tmp1$grts_cell_id[i],
                     location1 = tmp1$location_name[i],
                     location2 = tmp2$location_name,
                     dist = st_distance(tmp1[i,],tmp2, by_element = TRUE))
  out <- rbind(out,tmp3)
}

for(i in 1:length(out$grts)){
  out$pair[i] <- paste(sort(c(out$location1[i],out$location2[i]))[1],sort(c(out$location1[i],out$location2[i]))[2],sep = '_')
}

out <- out %>% 
  distinct(grts,pair,dist)

# Find distance between 2 specific locations
a <- '12162_SW2'
b <- 'PC'
out %>% 
  filter(str_detect(pair,a) &
           str_detect(pair,b))

out %>% 
  filter(str_detect(pair,'12162_SW3'))


### Pull coordinates to join to Crosswalk table
dat2 <- dat %>% 
  distinct(grts_cell_id,location_name,as.character(Latitude),as.character(Longitude))

write_csv(dat2,'C:/Users/emblidgp/Desktop/USFWS_NABat_Location_Coordinates.csv')
