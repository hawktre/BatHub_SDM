## ---------------------------
##
## Script name: 05_Application
##
## Purpose of script: Worked example for prediction raster
##
## Author: Trent VanHawkins
##
## Date Created: 2024-04-04
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## ---------------------------

## view outputs in non-scientific notation

options(scipen = 6, digits = 4) 

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(here)
require(terra)
require(tidyterra)
require(sf)
require(gt)


# Read in results and region ----------------------------------------------
block.res <- rast(here("DataProcessed.nosync/ModResults/all_blockres.tiff"))
odfw <- sf::read_sf(here("DataRaw.nosync/Application/ODFW_805_5_ODFW_wildlife_mgmt_units_shapefile/ODFW_wildlife_mgmt_units.shp"))


# Projeect, crop, and mask ------------------------------------------------
odfw <- st_transform(odfw, crs = st_crs(block.res))

deschutes <- odfw %>% filter(UNIT_NAME == "UPPER DESCHUTES")

deschutes_cp <- terra::crop(block.res, deschutes)

deschutes_mask <- terra::mask(deschutes_cp, deschutes)

pahe.wmu <- ggplot()+
  geom_spatraster(data = block.res$`Parastrellus hesperus`)+
  facet_wrap(~lyr)+
  geom_sf(data = deschutes, col = "red", fill = "red")+
  scale_fill_viridis_c(na.value = "transparent")+
  labs(fill = "Occ. Prob.")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

ggsave(filename = "pahe_WMU.png", plot = pahe.wmu, path = here("Reports/SDM_Presentation/Figures"),  width = 3024, height = 3024, units = "px")

# Plot --------------------------------------------------------------------
des.plot <- ggplot()+
  geom_spatraster(data = deschutes_mask) +
  facet_wrap(~lyr)+
  scale_fill_viridis_c(na.value = "transparent")+
  labs(fill = "Occ. Prob.")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

ggsave(filename = "des_plt.png", plot = des.plot, path = here("Reports/SDM_Presentation/Figures"),  width = 3024, height = 3024, units = "px")


# Compute the median for that region --------------------------------------
f <- function(x) c(median(x, na.rm = T))

manage_tab <- terra::global(deschutes_mask, fun = f) %>% as.data.frame() %>% 
  mutate(Threshold = 0.5,
         "Manage" = if_else(global> Threshold, "yes", "no")) %>% 
  dplyr::rename("Median" = "global")

manage_tab %>% 
  gt(rownames_to_stub = T) %>% 
  gt::fmt_number(decimals = 2, columns = 2) %>% 
  opt_row_striping()
  
