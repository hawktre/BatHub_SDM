# packages
packs <- c("sf", "tidyverse")
sapply(packs, require, character.only = T)
rm(packs)

# read data
data <- readr::read_csv("C:/Users/emblidgp/Downloads/NABat_Data_Export.csv")

# use the sf package to read in the WKB
## first, need to structure the character string as WKB
## example with a single entry - note geometry type is "polygon"
data$grts_geometry[1]
st_as_sfc(structure(data$grts_geometry[1], class = "WKB"), EWKB = TRUE)

# pipe through the whole data - will likely take a second or two if you lots of rows
data2 <- data %>%
  mutate(geometry = st_as_sfc(structure(grts_geometry, class = "WKB"), EWKB = TRUE)) %>%
  st_as_sf()

# plot for sanity check - more or less look like NABat cells
data2 %>%
  ggplot() + 
  geom_sf()

# can do the same thing for location geometry - note geometry type is "point"
data$location_geometry[1]
st_as_sfc(structure(data$location_geometry[1], class = "WKB"), EWKB = TRUE)

# can do the same thing for location geometry - note geometry type is "point"
data$wkb[1]
st_as_sfc(structure(data$wkb[1], class = "WKB"), EWKB = TRUE)
