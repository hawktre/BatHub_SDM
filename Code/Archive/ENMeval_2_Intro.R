# Load packages -- the order here is important because some pkg functions overwrite others.
library(ENMeval)
library(raster)
library(dplyr)

# Set a random seed in order to be able to reproduce this analysis.
set.seed(48)

# You can search online databases like GBIF using the spocc package (commented below),
# but here we will load in some pre-downloaded data.
# bv <- spocc::occ('Bradypus variegatus', 'gbif', limit=300, has_coords=TRUE)
# occs <- as.data.frame(bv$gbif$data$Bradypus_variegatus[,2:3])
data("bvariegatus")
occs <- bvariegatus

# Removing occurrences that have the same coordinates is good practice to
# avoid pseudoreplication.
occs <- occs[!duplicated(occs),]


# Curate WorldClim Data from "dismo" package ------------------------------

# Locate the predictor raster files from the dismo folder.
envs.files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), 
                         pattern='grd', full.names=TRUE)

# Read the raster files into a RasterStack.
# These variables represent 8 bioclimatic variables and one categorical variable "biome".
# Find the descriptions of the bioclimatic variables here: 
# https://www.worldclim.org/data/bioclim.html
envs <- raster::stack(envs.files)
# The biome raster has some NAs for cells that have values in the other rasters.
# Let's mask all rasters to biome to change the value of these cells to NA for all rasters.
# ENMeval will do this automatically, but let's do it here to avoid the warning message later.
# We change back from a RasterBrick to RasterStack because of issues with assigning 
# factor rasters for RasterBricks.
envs <- raster::mask(envs, envs[[9]]) %>% raster::stack()
# Make sure to declare the categorical variable as a factor
envs$biome <- raster::as.factor(envs$biome)
# Let's now remove occurrences that are cell duplicates -- these are
# occurrences that share a grid cell in the predictor variable rasters.
# Although Maxent does this by default, keep in mind that for other algorithms you may
# or may not want to do this based on the aims of your study.
# Another way to space occurrence records a defined distance from each other to avoid
# spatial autocorrelation is with spatial thinning (Aiello-Lammens et al. 2015).
occs.cells <- raster::extract(envs[[1]], occs, cellnumbers = TRUE)
occs.cellDups <- duplicated(occs.cells[,1])
occs <- occs[!occs.cellDups,]

# Plot first raster in the stack, the mean annual temperature.
plot(envs[[1]], main="Mean annual temperature")

# Add points for all the occurrence points onto the raster.
points(occs)

# There are some points east of the Amazon River.
# Suppose we know this is a population that we don't want to include in the model.
# We can remove these points from the analysis by subsetting the occurrences by 
# latitude and longitude.
occs <- filter(occs, latitude > -20, longitude < -45)

# Plot the subsetted occurrences to make sure we filtered correctly.
points(occs, col = 'red')


# Dissimilarity Map -------------------------------------------------------

# First we extract the climatic variable values at the occurrence points -- these values are 
# our "reference".
# We remove the categorical variable for these operations because the math only makes sense 
# for continuous variables -- the function will not work with categorical variables.
occs.z <- raster::extract(envs[[-9]], occs)
# Now we use the similarity() function (borrowed from the rmaxent package) to calculate 
# environmental similarity metrics of our predictor variable extent compared to the reference
# points.
occs.sim <- similarity(envs[[-9]], occs.z)
occs.mess <- occs.sim$similarity_min
# This is the MESS plot -- increasingly negative values represent increasingly different 
# climatic conditions from the reference (our occurrences), while increasingly positive 
# values are more similar. First, we'll make a SpatialPoints object for our occurrences 
# for plotting with levelplot() from the rasterVis package (Lamigueiro & Hijmans 2021).
# This package has great plotting functionality for rasters, and by default bins values for 
# display when data is continuous.
occs.sp <- sp::SpatialPoints(occs)
# Vector data (points, polygons) are added to a levelplot with a "+", like ggplot.
rasterVis::levelplot(occs.mess, main = "Environmental similarity", margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# Continuous plotting can be done as demonstrated below by specifiying a scale
myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# Here we define some good colors for representing categorical variables
cols <- RColorBrewer::brewer.pal(8, "Set1")
# This map shows the variable for each grid cell that is most different from the reference
rasterVis::levelplot(occs.sim$mod, col.regions = cols, main = "Most different variable") + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# This map shows the variable for each grid cell that is most similar to the reference
rasterVis::levelplot(occs.sim$mos, col.regions = cols, main = "Most similar variable") + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))


# Defining Background Extent ----------------------------------------------

# We'll now experiment with a different spatial R package called sf (simple features).
# Let's make our occs into a sf object -- as the coordinate reference system (crs) for these 
# points is WGS84, a geographic crs (lat/lon) and the same as our envs rasters, we specify it 
# as the RasterStack's crs.
occs.sf <- sf::st_as_sf(occs, coords = c("longitude","latitude"), crs = raster::crs(envs))

# Now, we project our point data to an equal-area projection, which converts our 
# degrees to meters, which is ideal for buffering (the next step). 
# We use the typical Eckert IV projection.
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)

# Buffer all occurrences by 500 km, union the polygons together 
# (for visualization), and convert back to a form that the raster package 
# can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
# We choose 500 km here to avoid sampling the Caribbean islands.
occs.buf <- sf::st_buffer(occs.sf, dist = 500000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(envs))
plot(envs[[1]], main = names(envs)[1])
points(occs)
# To add sf objects to a plot, use add = TRUE
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


# Crop environmental rasters to match the study extent
envs.bg <- raster::crop(envs, occs.buf)
# Next, mask the rasters to the shape of the buffers
envs.bg <- raster::mask(envs.bg, occs.buf)
plot(envs.bg[[1]], main = names(envs)[1])
points(occs)
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


# Sample Background Points ------------------------------------------------

# Randomly sample 10,000 background points from one background extent raster 
# (only one per cell without replacement). Note: Since the raster has <10,000 pixels, 
# you'll get a warning and all pixels will be used for background. We will be sampling 
# from the biome variable because it is missing some grid cells, and we are trying to 
# avoid getting background points with NA. If one raster in the stack has NAs where the
# other rasters have data, ENMeval internally converts these cells to NA.
bg <- dismo::randomPoints(envs.bg[[9]], n = 10000) %>% as.data.frame()
colnames(bg) <- colnames(occs)

# Notice how we have pretty good coverage (every cell).
plot(envs.bg[[1]])
points(bg, pch = 20, cex = 0.2)


# Partitioning ossc and Background Points (Spatial Block) -----------------


block <- get.block(occs, bg, orientation = "lat_lon")
# Let's make sure that we have an even number of occurrences in each partition.
table(block$occs.grp)
# We can plot our partitions on one of our predictor variable rasters to visualize 
# where they fall in space.
# The ENMeval 2.0 plotting functions use ggplot2 (Wickham 2016), a popular plotting 
# package for R with many online resources.
# We can add to the ggplots with other ggplot functions in an additive way, making 
# these plots easily customizable.
evalplot.grps(pts = occs, pts.grp = block$occs.grp, envs = envs.bg) + 
  ggplot2::ggtitle("Spatial block partitions: occurrences")

# PLotting the background shows that the background extent is partitioned in a way 
# that maximizes evenness of points across the four bins, not to maximize evenness of area.
evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = envs.bg) + 
  ggplot2::ggtitle("Spatial block partitions: background")


# If we are curious how different the environment associated with each partition is from 
# that of all the others, we can use this function to plot histograms or rasters of MESS 
# predictions with each partition as the reference.
# First we need to extract the predictor variable values at our occurrence and 
# background localities.
occs.z <- cbind(occs, raster::extract(envs, occs))
bg.z <- cbind(bg, raster::extract(envs, bg))

## Continuous
evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, bg.grp = block$bg.grp, categoricals = "biome")

## Categorical (different)
evalplot.envSim.hist(sim.type = "most_diff", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, bg.grp = block$bg.grp, categoricals = "biome")

## Categorical (Most Similar)
evalplot.envSim.hist(sim.type = "most_sim", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, bg.grp = block$bg.grp, categoricals = "biome")


# Here we plot environmental similarity values for the entire extent with respect 
# to each validation group.
# We use the bb.buf (bounding box buffer) argument to zoom in to our study extent.
evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

evalplot.envSim.map(sim.type = "most_diff", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

evalplot.envSim.map(sim.type = "most_sim", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)


# ENMevaluate -------------------------------------------------------------

## Linear Features only, RM values 1 & 2 (2 models)
e.mx.l <- ENMevaluate(occs = occs, envs = envs, bg = bg, 
                      algorithm = 'maxnet', partitions = 'block', 
                      tune.args = list(fc = "L", rm = 1:2))

e.mx.l

## Linear, Quadratic, and Hinge models w/ rm values 1:5
e.mx <- ENMevaluate(occs = occs, envs = envs, bg = bg, 
                    algorithm = 'maxnet', partitions = 'block', 
                    tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))
## Run in parallel
e.mx.p <- ENMevaluate(occs = occs, envs = envs, bg = bg, 
                      algorithm = 'maxnet', partitions = 'block', 
                      tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5),
                      parallel = T)


# Calculate overlap of models ---------------------------------------------


overlap <- calc.niche.overlap(e.mx@predictions, overlapStat = "D")

overlap

#Plot evaluation results

# Visualizing tuning results ----------------------------------------------

# We can also fiddle with the dodge argument to jitter the positions of overlapping points.
evalplot.stats(e = e.mx, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", 
               dodge = 0.5)


# Selecting the optimal model ---------------------------------------------

# Overall results
res <- eval.results(e.mx)
# Select the model with delta AICc equal to 0, or the one with the lowest AICc score.
# In practice, models with delta AICc scores less than 2 are usually considered 
# statistically equivalent.
opt.aicc <- res %>% filter(delta.AICc == 0)
opt.aicc
# This dplyr operation executes the sequential criteria explained above.
opt.seq <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq


# Choose best model and examine response curves ---------------------------

# We can select a single model from the ENMevaluation object using the tune.args of our
# optimal model.
mod.seq <- eval.models(e.mx)[[opt.seq$tune.args]]
# Here are the non-zero coefficients in our model.
mod.seq$betas
# And these are the marginal response curves for the predictor variables wit non-zero 
# coefficients in our model. We define the y-axis to be the cloglog transformation, which
# is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
# (Phillips et al. 2017).
plot(mod.seq, type = "cloglog")

dev.off()


# Spatial Results ---------------------------------------------------------

# We can select the model predictions for our optimal model the same way we did for the 
# model object above.
pred.seq <- eval.predictions(e.mx)[[opt.seq$tune.args]]
plot(pred.seq)

# We can also plot the binned background points with the occurrence points on top to 
# visualize where the training data is located.
points(eval.bg(e.mx), pch = 3, col = eval.bg.grp(e.mx), cex = 0.5)
points(eval.occs(e.mx), pch = 21, bg = eval.occs.grp(e.mx))
