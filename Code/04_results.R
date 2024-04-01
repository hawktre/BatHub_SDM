## ---------------------------
##
## Script name: 04_results.R
##
## Purpose of script: Load in results of maxent model and interpret
##
## Author: Trent VanHawkins
##
## Date Created: 2024-03-08
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
require(ENMeval)
require(leaflet)
require(tidyterra)

# Read in the results -----------------------------------------------------
all.res <- readRDS(here("DataProcessed.nosync/ModResults/all_res.rds"))
block.res <- readRDS(here("DataProcessed.nosync/ModResults/block_res.rds"))

wbwg <- readxl::read_xlsx(here("Background/SpeciesNatHistMatrix.xlsx"), sheet = 2) %>% 
  drop_na(fourlettername)

# Select the best models by ommission rate and AUC ------------------------
#Create the function
get.res <- function(mods){
  #get generic results object from ENMeval
  res <- eval.results(mods)
  #Get the object with lowest ommission rate and higher AUC to break ties
  opt.seq <- res %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>% 
    filter(auc.val.avg == max(auc.val.avg))
  
  return(opt.seq)
}


best.mods.kmeans <- lapply(all.res, get.res)
best.mods.block <- lapply(block.res, get.res)


# Create rasters for selected best models ---------------------------------
kmeans.rast <- rast()
block.rast <- rast()

for (i in 1:length(all.res)) {
  
  
  #Extract the best result by ommission rate and AUC
  terra::add(kmeans.rast) <- rast(all.res[[i]]@predictions[[best.mods.kmeans[[i]]$tune.args]])
  terra::add(block.rast) <- rast(block.res[[i]]@predictions[[best.mods.block[[i]]$tune.args]])
  
}

## fix the names of the rasters
names(kmeans.rast) <- names(all.res)
names(block.rast) <- names(block.res)

## Set the colors for the raster
pal <- colorNumeric(c(viridis::viridis(20)), values(block.rast[["anpa"]]),
                    na.color = "transparent")

## Fix names to display scientific name
names_map <- as.data.frame(names(kmeans.rast))
names(names_map) <- c("fourlettername")

names_map <- names_map %>% 
  left_join(wbwg %>% select(fourlettername, SPECIES, `SUM ROOST`), by = "fourlettername")

## Fix names in rasters
names(kmeans.rast) <- names_map$SPECIES
names(block.rast) <- names_map$SPECIES

generalists <- wbwg$SPECIES[which(wbwg$`SUM ROOST` == "GENERAL")]
cliff_cave <- wbwg$SPECIES[which(wbwg$`SUM ROOST` == "CLIFF")]
tree <- wbwg$SPECIES[which(wbwg$`SUM ROOST` == "TREES")]
other <- wbwg$SPECIES[which(wbwg$`SUM ROOST` == "OTHER")]


# Creating Plots ----------------------------------------------------------

plot.preds <- function(res.type, spp.group){
  ggplot()+
    geom_spatraster(data = res.type[[spp.group]])+
    facet_wrap(~lyr)+
    scale_fill_viridis_c(na.value = "transparent")+
    labs(fill = "Occ. Prob.")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
}

## generalists
general.kmeans <- plot.preds(kmeans.rast, generalists)
general.block <- plot.preds(block.rast, generalists)

ggsave(filename = "general_res.png", plot = general.block, here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

## Cliff/Canyon
cliff.kmeans <- plot.preds(kmeans.rast, cliff_cave)
cliff.block <- plot.preds(block.rast, cliff_cave)

ggsave(filename = "cliffcanyon_res.png", plot = cliff.block, here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

## Trees
trees.kmeans <- plot.preds(kmeans.rast, tree)
trees.block <- plot.preds(block.rast, tree)

ggsave(filename = "trees_res.png", plot = trees.block, here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

## Other
other.kmeans <- plot.preds(kmeans.rast, other)
other.block <- plot.preds(block.rast, other)

ggsave(filename = "other_res.png", plot = other.block, here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")



