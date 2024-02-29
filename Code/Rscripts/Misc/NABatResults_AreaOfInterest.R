library(sf)
library(ggplot2)

aoi <- st_read("C:/Users/emblidgp/Downloads/ODFW_Bat_AOI/Bat_AOI.shp")
SUs <- st_read('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.shp')

aoi <- st_transform(aoi, 4326)
SUs <- st_transform(SUs, 4326)



ggplot() + 
  geom_sf(data = SUs, size = 1.5, color = "blue") + 
  geom_sf(data = aoi, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("TestPlot") + 
  coord_sf()




SUs <- SUs %>% 
  st_filter(aoi, .predicate = st_intersects) %>% 
  mutate(GRTS_ID = as.character(GRTS_ID))

### Occurrence Probability
dat <- read_csv("C:/Users/emblidgp/Downloads/EPFU_gridcell_occupancy.csv")
dat2 <- dat %>% 
  filter(str_detect(grts,"US") &
           year == 2019) %>% 
  mutate(grts = str_remove(grts,"US")) %>% 
  select(grts,mean) %>% 
  filter(grts %in% SUs$GRTS_ID) %>% 
  rename("epfu" = "mean")


dat <- read_csv("C:/Users/emblidgp/Downloads/LACI_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
  filter(str_detect(grts,"US") &
           year == 2019) %>% 
  mutate(grts = str_remove(grts,"US")) %>% 
  select(grts,mean) %>% 
  filter(grts %in% SUs$GRTS_ID) %>% 
  rename("laci" = "mean"),
  by = 'grts')

dat <- read_csv("C:/Users/emblidgp/Downloads/LANO_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
              filter(str_detect(grts,"US") &
                       year == 2019) %>% 
              mutate(grts = str_remove(grts,"US")) %>% 
              select(grts,mean) %>% 
              filter(grts %in% SUs$GRTS_ID) %>% 
              rename("lano" = "mean"),
            by = 'grts')

dat <- read_csv("C:/Users/emblidgp/Downloads/MYEV_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
              filter(str_detect(grts,"US") &
                       year == 2019) %>% 
              mutate(grts = str_remove(grts,"US")) %>% 
              select(grts,mean) %>% 
              filter(grts %in% SUs$GRTS_ID) %>% 
              rename("myev" = "mean"),
            by = 'grts')

dat <- read_csv("C:/Users/emblidgp/Downloads/MYLU_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
              filter(str_detect(grts,"US") &
                       year == 2019) %>% 
              mutate(grts = str_remove(grts,"US")) %>% 
              select(grts,mean) %>% 
              filter(grts %in% SUs$GRTS_ID) %>% 
              rename("mylu" = "mean"),
            by = 'grts')

dat <- read_csv("C:/Users/emblidgp/Downloads/MYTH_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
              filter(str_detect(grts,"US") &
                       year == 2019) %>% 
              mutate(grts = str_remove(grts,"US")) %>% 
              select(grts,mean) %>% 
              filter(grts %in% SUs$GRTS_ID) %>% 
              rename("myth" = "mean"),
            by = 'grts')

dat <- read_csv("C:/Users/emblidgp/Downloads/MYVO_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
              filter(str_detect(grts,"US") &
                       year == 2019) %>% 
              mutate(grts = str_remove(grts,"US")) %>% 
              select(grts,mean) %>% 
              filter(grts %in% SUs$GRTS_ID) %>% 
              rename("myvo" = "mean"),
            by = 'grts')

dat <- read_csv("C:/Users/emblidgp/Downloads/MYYU_gridcell_occupancy.csv")
dat2 <- dat2 %>% 
  left_join(dat %>% 
              filter(str_detect(grts,"US") &
                       year == 2019) %>% 
              mutate(grts = str_remove(grts,"US")) %>% 
              select(grts,mean) %>% 
              filter(grts %in% SUs$GRTS_ID) %>% 
              rename("myyu" = "mean"),
            by = 'grts')

dat2

### SampleUnits clipped by area of interest polygon
SUs <- SUs %>% 
  left_join(dat2,
            by = c('GRTS_ID'='grts'))

SUs2 <- SUs %>% 
  st_intersection(aoi)

### Calculate mean occurrence probability, weighted by area, for simple feature collection
data.frame(Species = c('epfu','laci','lano','myev','mylu','myth','myvo','myyu'),
           Mean = c(mean(SUs2$epfu),
                    mean(SUs2$laci),
                    mean(SUs2$lano),
                    mean(SUs2$myev),
                    mean(SUs2$mylu),
                    mean(SUs2$myth),
                    mean(SUs2$myvo),
                    mean(SUs2$myyu)),
           WeightedMean = c(weighted.mean(SUs2$epfu,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$laci,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$lano,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$myev,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$mylu,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$myth,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$myvo,as.numeric(st_area(SUs2))),
                            weighted.mean(SUs2$myyu,as.numeric(st_area(SUs2)))))

ggplot() + 
  geom_sf(data = SUs, size = 1, color = "black") + 
  geom_sf(data = SUs2, size = 2, color = "blue") + 
  ggtitle("TestPlot") + 
  coord_sf()
