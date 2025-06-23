# data_grid.R - DESC
# 2025_tur.27.4_benchmark/data_grid.R

# Copyright (c) WUR, 2024.
# Author: Klaas SYS <klaas.sys@ilvo.vlaanderen.be>
#         Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#         Justin TIANO (WMR) <justin.tiano.wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(sf)
library(stars)
library(surveyIndex)

# GET ICES area shapes, https://gis.ices.dk/sf/
shp <- read_sf('boot/data/icesareas/ICES_Areas_20160601_cut_dense_3857.shp')

# SUBSET North sea
shp_sub <- shp[shp$Area_27 %in% c("4.a","4.b","4.c"),]  

# check validity of shapefiles
if(any(!st_is_valid(shp_sub))){
  shp_sub <- st_make_valid(shp_sub)
}

# PROJECT to lon/lat
shp_sub <- st_transform(shp_sub, crs = 4326)   # reproject to lon lat

# REDUCE
shp_sub_reduced <- st_simplify(st_union(shp_sub), dTolerance = 50000)

# get bounding box to define grid
bbox  <- st_bbox(shp_sub_reduced)

lon_range <- c(floor(bbox["xmin"] * 10) / 10, ceiling(bbox["xmax"] * 10) / 10)
lat_range <- c(floor(bbox["ymin"] * 10) / 10, ceiling(bbox["ymax"] * 10) / 10)

grid_bbox <- data.frame(lon = lon_range, lat = lat_range)

# add depth from NOAA webservices
grid <- getBathyGrid(grid_bbox, resolution = 15) # approx 0.25 0.25 degrees

# convert back to sf
grid <- st_as_sf(grid, coords = c("lon","lat"), crs = 4326) 

# rasterize
raster          <- st_rasterize(grid)

# intersect
grid <- st_as_sf(raster) %>%
  st_intersection(shp_sub_reduced) 

# add area
grid$Area  <- st_area(grid)
grid$Area  <- units::set_units(grid$Area, km^2)

# compute midpoints of polygons
grid$centroid <- st_centroid(grid)

# parse into a data.frame
grid <- cbind(as.data.frame(st_coordinates(grid$centroid$geometry)),
  grid[,c("Depth","Area")])
grid$geometry <- NULL
colnames(grid)[1:2] <- c("lon","lat")

# nsm + geom_point(data=grid, aes(x=lon, y=lat), alpha=0.2, size=1)

# eventually remove grid cells with small areas or depth = 0 or depth > threshold

grid <- subset(grid, Depth > 10)

# remove units (not sure if surveyIndex will handle units)
grid$Area <- as.numeric(grid$Area)

# create input grid for surveyIndex
#-----------------------------------------
years  <- 1991:2024
myShip <- NA # enter Ship code
myGear <- NA # enter a valid Gear code
grid  <- lapply(years, function(x,grid, myShip, myGear){
  grid$Year=factor(x) 
  grid$dum=0;
  # BUG: SweptAtea*
  grid$SweptArea = grid$Area   # check the name of the sweptarea columns
  grid$Ship= myShip
  grid$Gear= myGear
  grid$HaulDur=30.0
  grid
},grid = grid, myShip = myShip, myGear = myGear)
names(grid) <- years

# Specify grid for North Sea scientific surveys
nsgrid <- grid

##################################
## BSAS grid ##
##################################

load('data/grid.rda') # Load if needed only

# -- BSAS grid

load('data/surveysAll.RData')

dat.mod <- subset(all, Survey %in% c('NL-BSAS'))

hauls <- dat.mod[["HH"]][,c("lon","lat")]

# create a buffer around the hauls
sf_hauls    <- st_as_sf(hauls, coords = c("lon","lat"), crs = 4326)
hull        <- st_convex_hull(st_union(sf_hauls)) 
hull_buffer <- st_convex_hull(st_buffer(st_union(sf_hauls), 2500)) 

# PLOT
plot(st_union(shp_sub), border = "grey60")
plot(shp_sub_reduced, add = T)
plot(hull, add = T, border = "blue")
plot(hull_buffer, add = T, border = "red")
plot(st_multipoint(as.matrix(hauls)), add = T, border = "green")
plot(st_intersection(st_union(shp_sub), hull_buffer), add = T, border = "purple", lwd = 4)

shp_sub_reduced <- st_intersection(st_union(shp_sub), hull_buffer)

# create grid
#-----------------------------------------

# get bounding box to define grid
bbox  <- st_bbox(shp_sub_reduced)

lon_range <- c(floor(bbox["xmin"]*10)/10,ceiling(bbox["xmax"]*10)/10)
lat_range <- c(floor(bbox["ymin"]*10)/10,ceiling(bbox["ymax"]*10)/10)
grid_bbox <- data.frame(lon = lon_range, lat = lat_range)

# add depth from NOAA webservices
grid2 <- surveyIndex::getBathyGrid(grid_bbox, resolution = 6) # approx 0.1 0.1 degrees
# for some reason, the resolution never matches a 100%

# convert back to sf
grid2 <- st_as_sf(grid2, coords = c("lon","lat"), crs = 4326) 

# rasterize
raster          <- st_rasterize(grid2)

# intersect
grid2 <- st_as_sf(raster) %>%
  st_intersection(shp_sub_reduced) 
# add area
grid2$Area  <- st_area(grid2)
grid2$Area  <- units::set_units(grid2$Area, km^2)

# compute midpoints of polygons
grid2$centroid <- st_centroid(grid2)

# parse into a data.frame
grid2 <- cbind(as.data.frame(st_coordinates(grid2$centroid$geometry)),grid2[,c("Depth","Area")])
grid2$geometry <- NULL
colnames(grid2)[1:2] <- c("lon","lat")

# PLOT
plot(grid2[grid2$Depth == 0,c("lon","lat")])
plot(grid2[as.numeric(grid2$Area) < 50,c("lon","lat")])
# eventually remove grid cells with small areas or depth = 0 or depth > threshold
plot(grid2[,c("lon","lat")])

grid <- grid2

# remove units (not sure if surveyIndex will handle units)
grid$Area <- as.numeric(grid$Area)

bsasgrid <- grid2

save(nsgrid, bsasgrid, file = "data/grid.rda")

unlink(list.files(pattern='marmap*'))
