## ----setup, include=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=F, message=F---------------------------------------------------------
# Load libraries
library(sf)
library(terra)
library(dplyr)
library(tidyverse)
library(lubridate)
library(gt)
library(tmap)


## ---------------------------------------------------------------------------------
plot(st_point(c(0,0)))
plot(st_point(c(0.5,0.5)), add = T)


## ---------------------------------------------------------------------------------
# Generate a random linestring...6 numbers across 2 columns
plot(st_linestring(matrix(runif(6), ncol=2)) )


## ---------------------------------------------------------------------------------
plot(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))


## ---------------------------------------------------------------------------------
# Import Kenya Major Towns CSV File.  Just a flat dataframe (non-spatial)
my_df <- read.csv("Data/ke_major-towns.csv", header = TRUE)
head(my_df)

# Create sf object with geo_data data frame and CRS.  I.e., Make the data spatial.
points_sf <- st_as_sf(my_df, coords = c("lon", "lat"), crs = 4326)
print(points_sf)
#plot(points_sf, pch = 18, cex=1)


## ----warning=F, message=F---------------------------------------------------------
points_sf_UTM <- st_transform(points_sf, crs="+proj=utm +zone=37 +north +ellps=WGS84 +datum=WGS84 +units=m")
points_sf_UTM


## ----warning=F, message=F---------------------------------------------------------
ke_mammals <- st_read("Data/ke_mammals_WGS84.shp")
ke_mammals


## ----warning=F, message=F---------------------------------------------------------
# Look at the data
plot(ke_mammals)

# Create a dataset subset of the polygons with > 60 mammals
mammals_60 <- ke_mammals %>% 
  rename_with(tolower) %>% # This function renames all column headers...in this case to lowercase
  filter(mammals > 60)

# Print
#nrow(mammals_60)
mammals_60


## ----warning=F, message=F, echo=F, eval=F-----------------------------------------
## # Look at the head and structure
## head(mammals_60)
## str(mammals_60)
## 
## # Plot the result
## plot(mammals_60)


## ----warning=F, message=F---------------------------------------------------------
# Read the image
Image <- rast("Data/Landsat8.tif")
Image


## ----warning=F, message=F---------------------------------------------------------
# Summarize the object
print(Image)

# What is the class of the object?
class(Image)

# Summarize the bands included in the stack
names(Image)

# Provide summary statistics of each band
summary(Image)

# What is the extent of the image?
ext(Image)


## ----message=F, warning=F---------------------------------------------------------
# Obtain the Coordinate Reference System of the image
crs(Image, proj=T)

# If the projection of the image was null, we'd have to set it
#crs(Image) <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"

# To Reproject an image, we'd do the following:
# Imagelatlong <- project(Image, "+init=epsg:4326", method = 'bilinear')
# crs(Imagelatlong, proj = T)


## ---------------------------------------------------------------------------------
# Plot an image.  Here, I am only plotting band 4.
plot(Image, 4)

# Or, use plotRGB to plot a 3-band image
plotRGB(Image, r=4, g=3, b=2, stretch="lin", axes=T, xlab="Easting", ylab="Northing", main="Landsdat 8, Bands 4,3,2 (RGB) - True Color Composite")


## ----warning=F, message=F, echo=F, eval=F-----------------------------------------
## # Plot a 3-band RGB image in false color composite
## plotRGB(Image, r=5, g=4, b=3, stretch = "lin", axes=TRUE, xlab="Easting", ylab="Northing", main="Landsat 8, Bands 5,4,3 (RGB) - False Color Composite")


## ----warning=F, message=F---------------------------------------------------------
# Using standard math operators, rename the red band (band 3) and the near-infrared band (band 4) in the image.
redBand <- Image$B4
nirBand <- Image$B5

# Create NDVI
ndvi <- (nirBand-redBand)/(nirBand+redBand)

# Rename the bank
names(ndvi) <- "NDVI"

# Plot the image
plot(ndvi)

# Display a histogram of values
hist(ndvi)


## ----warning=F, message=F---------------------------------------------------------
# Create random samples (pixels) from a raster
ranSamps <- st_sample(st_as_sfc(st_bbox(ndvi)), size = 10, type = "random") # This step is skipped if you have a file that overlaps with your raster
# Convert to a simple feature
ranSamps <- st_as_sf(ranSamps) # Note that I have overwritten the initial object
# Print
ranSamps

# Plot the random points on top of our NDVI image
plot(ndvi)
plot(ranSamps, add=T)

# Now Extract
ndvi.vals <- terra::extract(ndvi, ranSamps)  # We need to be explicit that extract comes from the terra package here
ndvi.vals


## ----warning=F,message=F,echo=F,eval=F--------------------------------------------
## writeRaster(ndvi, "Output/NDVI.tif", overwrite=TRUE)


## ----warning=F, message=F---------------------------------------------------------
# Load Fence data layer - Data layer is Geographic 
Fences <- st_read("Data/Fences_LandDx.shp") %>%
  select(type, active) %>% # Select the type of fence and whether it is active
  st_transform(crs = 32736) # Transform to UTM 36S WGS84

# Read in the Conservancy data layer - Data layer is projected to UTM36S
Mara_Cons <-  st_read("Data/MaraConservancies.shp") %>% 
  select(objectid, short_name, Shape_Leng, Shape_Area) # Columns to select
  
# Read in the primary roads data layer - Data layer is Geographic 
Roads <- st_read("Data/PrimaryRoads.shp") %>% 
  select(type, name, active) %>% # Columns to select
  st_transform(crs = 32736) # Transform to UTM 36S WGS

# Read in the bomas data layer - Data layer is Geographic 
Bomas <- st_read("Data/bomas_LandDx.shp") %>% 
  select(short_name) %>% # Columns to select
  st_transform(crs = 32736) # Transform to UTM 36S WGS84

# Read in the towns data layer - Data layer is Geographic
Towns <- st_read("Data/MajorTowns.shp") %>% 
  st_transform(crs = 32736) # Transform to UTM 36S WGS84

# Read in the Reserve boundary - Data layer is projected to UTM36S
MMNR <- st_read("Data/MMNR.shp") %>% 
    select(NAME)


## ----warning=F, message=F---------------------------------------------------------
# Create a quick plot of some of the data layers loaded.
# Do they overlap?
par(mfrow = c(1,2))
plot(st_geometry(Mara_Cons), border = 'dark green', col = 'light green', xlab = 'Easting', ylab = 'Northing', axes = TRUE)
plot(st_geometry(Fences), col = 'blue', add = TRUE) # Fences are a line file...we must use 'col' to color the lines
plot(st_geometry(Towns), col = 'red', cex = 0.5, pch = 17, add=TRUE)

plot(st_geometry(Mara_Cons), border = 'dark green', col = 'light green', xlab = 'Easting', ylab = 'Northing', axes = TRUE)
plot(st_geometry(MMNR), col = 'light green', border = 'dark green', add=TRUE) # MMNR is a polygon
plot(st_geometry(Towns), col = 'red', cex = 0.5, pch = 17, add=TRUE)
plot(st_geometry(Roads), col = 'black', add=TRUE)
par(mfrow = c(1,1))


## ----message=F,warning=F----------------------------------------------------------
# Complete a simple filter
Nab_Cons <- Mara_Cons %>% 
  filter(short_name == "Naboisho Conservancy")
Nab_Cons
#plot(Nab_Cons)

# Now, recalculate the area of all the conservancies in acres and select only those conservancies that are larger than 50,000 acres.  How many are there?
Big_Cons <- Mara_Cons %>% 
  mutate(acres = units::set_units(st_area(Mara_Cons),
                                  "acre")) %>% 
  filter(as.numeric(acres) > 50000)

# How many?
length(unique(Big_Cons$short_name))
# What are there names?
unique(Big_Cons$short_name)

# We could plot them if we wanted to
# plot(st_geometry(Mara_Cons), axes=T)
# plot(st_geometry(Big_Cons), col = 'green', add=T)


## ----warning=F,message=F----------------------------------------------------------
# Create a blank raster.  Set the conservancy boundary as the extent.
canvas <- rast(ext(Mara_Cons), crs=crs(Roads), resolution=50)

# Create a road raster by rasterizing the road vector layer.
road.raster <- rasterize(Roads, canvas) 

# Generate a distance raster
distance <- distance(road.raster)

# Plot the result
plot(distance) # Result is in meters
plot(Roads, add=TRUE)
plot(Bomas, add=TRUE, pch = 15, cex = 1, col = "red")

# Extract distance
dist.extract <- terra::extract(distance, Bomas)

# Summarize findings
summary(dist.extract$layer)
hist(dist.extract$layer)

# Or we could bind the distance layer back together with or Boma layer
#Bomas.dist <- cbind(Bomas, dist.extract)


## ----warning=F,message=F----------------------------------------------------------
# Read in the dataset
WB <- read.csv(file = "Data/wild_mara.csv", header = TRUE)

# Create character objects for the TimeZone
Timezone1 <- 'UTC'
Timezone2 <- "Africa/Nairobi"

# We'll overwrite the dataset, clean-up the columns, and make sure no duplicate positions exist 
WB <- WB %>%
  # Rename columns and create timestamp - UTC to local
  mutate(id = ID,
         animal_id = Name,
         deploy_id = paste(id,'-',animal_id), # Join these character fields together.
         latitude = Lat,
         longitude = Long,
         sex = Sex,
         age = Age,
         species = "White-bearded Wildebeest",
         DOP = DOP,
         fixType = substr(Nav,1,2),
         timestamp = dmy_hms(paste(date, time), tz=Timezone1), # Convert to date/time
         timestamp = with_tz(timestamp, tz=Timezone2),
         .keep = "none") %>% # This last command states to only keep the columns that I specifically create here.  Ignore all remainder columns.
  
  # Remove any records where the id and timestamp are duplicated
  distinct(animal_id, timestamp, .keep_all = TRUE) %>% 
  
  # Remove any records that don't have a timestamp or a Lat/Long location
  filter(!is.na(timestamp),
         !is.na(latitude),
         !is.na(longitude),
         latitude != 0, 
         longitude != 0) %>% 
  
  # Arrange the dataset by id and timestamp
  arrange(id, timestamp)

# Visualize the first 5 rows of the dataset
head(WB)


## ----warning=F, message=F---------------------------------------------------------
# Summarize results to a table
wb.Summary <- WB %>% 

  # Summarize the dataset to highlight the number of records and duration of tracking period
  summarise(
    Sex = unique(sex),
    Records = n(),
    StartDate = min(timestamp),
    EndDate = max(timestamp),
    TrackPeriod = round(difftime(max(timestamp),min(timestamp),units = "days"),digits=1),
    .by = id) %>% 
  
  # Arrange by start date
  arrange(id, StartDate)

# Print the file
#wb.Summary

# We could write this file to a csv, but let's make a more attractive table
#write.csv(wb.Summary,file = "Output/wbSummary.csv", quote=FALSE, row.names=FALSE)

# Create a gt formatted table
gt_wb <- wb.Summary %>% 
  # initialize gt
  gt() %>% 
  # row striping
  opt_row_striping() %>% 
  # Add title and header
  tab_header(
    title = "Wildebeest: Tracking Data Summary",
    subtitle = Sys.Date()) %>% 
  # Easy formatting
  fmt_date(
    columns = c(StartDate, EndDate),
    date_style = 8) %>% 
  cols_label(id = "Wildebeest ID",
             Sex = "Sex",
             Records = "Total points",
             StartDate = "First location",
             EndDate = "Last location",
             TrackPeriod = "Tracking period (d)") %>% 
  # center columns
  cols_align(align = "center")

gt_wb

# Save the table as an html file to your Output folder
gtsave(gt_wb, filename = "Output/Summary_wildebeest.html")


## ----warning=F, message=F---------------------------------------------------------
# Convert to a sf object
wb.pts <- st_as_sf(WB, coords = c("longitude", "latitude"), crs = 4326) # Geographic
wb.pts.utm <- st_transform(wb.pts, crs = 32736) # UTM 36S WGS84

# Convert the points to lines
wb.lines <- wb.pts.utm %>% 
  group_by(id) %>% 
  dplyr::summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")


## ----warning=F, message=F---------------------------------------------------------
# Create a tmap
tmap_mode('view') 

# Add basemaps to display underneath data files
Mara_proj <- tm_basemap(
  c('OpenStreetMap',
    'Esri.WorldTopoMap',
    'Esri.WorldImagery')
  ) +
  
  # Add the conservancy boundaries - Polygon
  tm_shape(Mara_Cons, 
           name = 'Conservancies') + # Name included in the legend
    tm_borders(col = 'green') + # Color of the borders
  
  tm_shape(MMNR,
           name = 'MMNR') +
    tm_borders(col = 'green') +
  
  # Show Naboisho from the subset (filter) made above
  tm_shape(Nab_Cons,
           name = 'Naboisho') +
    tm_fill(col = 'orange',
            alpha = 0.5) +
    tm_borders(col = 'orange') +
  
  # Add the Fences - MultiLines
  tm_shape(Fences) +
    tm_lines(lwd = 0.25, # Width of the lines
             col = 'blue') + # COlor of the lines
  
  # Add the Main Roads - MultiLines
  tm_shape(Roads,
           name = 'Roads') +
    tm_lines(col = "type",
             title.col = "Road Type") +

  # Load the Towns - change the symbol
  tm_shape(Towns) +
    tm_dots(size = 0.01,
            col = 'red') +
  
  # Load the wildebeest data - Points
  tm_shape(wb.pts.utm,
           name = 'Wildebeest Points') + # The name will appear in the legend
    tm_dots(col = "id", # COlor based on the id
            palette = viridis::viridis(length(unique(wb.pts.utm$id))), # A color palette
            size = 0.001, # Size of the points
            legend.show = FALSE) +
  
  # Load the wildebeest data - Lines
  tm_shape(wb.lines,
           name = "Wildebeest Lines") + # The name will appear in the legend
    tm_lines(lwd = 0.25, # Controls the width of the lines
             col = 'id', # Color based on the id
             palette = viridis::viridis(length(unique(wb.lines$id))), # A color palette
             legend.col.show = FALSE)

# Show the map   
Mara_proj

# Save the file
tmap_save(Mara_proj, filename = "Output/MaraMap.html")


## ----warning=F,message=F,echo=F,eval=F--------------------------------------------
## # Write wildebeest points to shapefile
## st_write(wb.pts.utm, "Output/wb_points.shp", append = FALSE)
## 
## # Write wildebeest lines to shapefile
## st_write(wb.lines, "Output/wb_lines.shp", append=FALSE)

