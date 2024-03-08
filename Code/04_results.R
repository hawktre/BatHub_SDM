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

# Read in the results -----------------------------------------------------
all.res <- readRDS(here("DataProcessed.nosync/ModResults/all_res.rds"))



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


best.mods <- lapply(all.res, get.res)


preds.rast <- rast()

for (i in 1:length(all.res)) {
  
  
  #Extract the best result by ommission rate and AUC
  terra::add(preds.rast) <- rast(all.res[[i]]@predictions[[best.mods[[i]]$tune.args]])
  
}

names(preds.rast) <- names(all.res)

pal <- colorNumeric(c(viridis::viridis(20)), values(preds.rast[["anpa"]]),
                    na.color = "transparent")

library(htmlwidgets)
m <- leaflet() %>% 
  addTiles() %>% 
  addRasterImage(x = preds.rast[["anpa"]], colors = pal, opacity = 0.5) %>% 
  addLegend(pal = pal, values = values(preds.rast[["anpa"]]), title = "Occurrence Prob.") 

m

saveWidget(m, file = here("DataProcessed.nosync/ModResults/test_widget.html"), selfcontained = T)
