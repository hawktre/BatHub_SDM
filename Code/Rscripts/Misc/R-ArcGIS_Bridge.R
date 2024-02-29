library(tidyverse)
library(ggplot2)
library(arcgisbinding)
library(sf)
arc.check_product() 

forests <- 'https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Administrative_Forest_Boundaries/FeatureServer' %>% 
  arc.open() %>% 
  arc.select()


t1 <- arc.open('https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Administrative_Forest_Boundaries/FeatureServer/0')
t2 <- arc.select(t1)
forests99 <- arc.data2sf(t2)

t1 <- arc.open('https://services1.arcgis.com/CD5mKowwN6nIaqd8/arcgis/rest/services/complete_conus_mastersample_10km/FeatureServer/0')
t2 <- arc.select(t1, where_clause = 'GRTS_ID IN (305,481,545,561)') # where_clause in SQL format
cells99 <- arc.data2sf(t2)

ggplot() +
  geom_sf(data = cells99,color ='blue') +
  geom_sf(data = forests99)

test <- st_cast(forests99, "POLYGON")

library("tmap")
tmap_mode("view")
tm_shape(PADUS) + tm_polygons()+
  tm_shape(forests99) + tm_polygons(col="FORESTNAME", legend.show=F, alpha = 0.3)

st_join(cells99,forests99)
st_filter(cells99,test, .predicate = st_intersects)


t1 <- st_read("C:/Users/emblidgp/Downloads/complete_conus_mastersample_10km-selected/complete_conus_mastersample_10km.shp")
t2 <- st_read("C:/Users/emblidgp/Downloads/Administrative_Forest_Boundaries/AdministrativeForest.shp")

st_crs(t1)
t1 <- st_transform(t1,4326)

t1 <- t1 %>% 
  filter(GRTS_ID %in% c(305,481,545,561))

st_join(t1,t2)
st_filter(t1,t2, .predicate = st_intersects)
