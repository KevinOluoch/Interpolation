
# PACKAGES ----------------------------------------------------------------
# If packages are missing run:
# install.packages(c("gstat", "raster", "RPostgres", "sf", "ggplot2"))

library(gstat)
library(raster) 
library(RPostgres)
library(sf)
library(ggplot2)

# NB: sp already loaded in the environment

# VIEW DATA ---------------------------------------------------------------
gadm
po

head(gadm@data)
head(po@data)

# Check if coordinate systems (exact coordinate string) are the same.
# This Avoids errors (or warnings) in downstream processes.
as.character(gadm@proj4string) == as.character(po@proj4string) # TRUE

#Convert to sf 
gadm.sf <- merge(st_as_sf(gadm), gadm@data, names(gadm))
po.sf <- merge(st_as_sf(po), po@data, names(po))


# Plot data
ggplot() + 
  geom_sf(data = gadm.sf,  colour="black", fill="ivory", size=0.2) +
  geom_sf(data = po.sf, color="blue", fill=NA, size=1, shape=16)



# CLEAN DATA --------------------------------------------------------------
# Since the data are of type SpatialPointsDataFrame SpatialPolygonsDataFrame,
# the coordinates have no NA values.

# Exclude entries with missing ph values
po <- po[which(!is.na(po$pH)),]

# Exclude entries with wrong values (outside the 0 to 14 PH scale)
po <- po[which(po$pH > 0 & po$pH < 14),]



# CREATE EMPTY RASTER AND SPATIAL GRID ------------------------------------

# Create parameters raster - raster with the required output properties.
paramsRaster <- 
  raster(extent(gadm), ncols=1000, nrows=1000, crs = proj4string(po))


# At The Equator (or any of Earth's great circles) 1 degree = 111,319.5 meteres
# Setting the output resolution to 100 m(value in decimal degrees)
res100mInDeg <- 100/111319.5 
res(paramsRaster) <- res100mInDeg
paramsRaster

# Convert the Study Area to a raster using parameters Raster
gadmEmptyRaster <- rasterize(gadm, paramsRaster)
gadmEmptyRaster
plot(gadmEmptyRaster)

# Create spatial grided spatial data
gadmEmptyCells <- as.data.frame(xyFromCell(gadmEmptyRaster, 
                                     1:ncell(gadmEmptyRaster)))

names(gadmEmptyCells)       <- c("X", "Y")
coordinates(gadmEmptyCells) <- c("X", "Y")
gridded(gadmEmptyCells)     <- TRUE  
fullgrid(gadmEmptyCells)    <- TRUE  
proj4string(gadmEmptyCells) <- proj4string(gadm)



# INTERPOLATE PH OVER THE SPATIAL GRID SURFACE ----------------------------

# Well use Inverse Distance Weight with distances to the power of 3
# for its suitability in interpolating PH values as described here 
# (https://www.researchgate.net/publication/317359134)

gadmPH <- idw(pH ~ 1, locations = po,  newdata=gadmEmptyCells, idp=3.0)

# mask the interpolated surface
# First remove interpolation points outside the sample points area 
# as they may be incorrect 
gadmPH.rst0 <- raster(gadmPH)
gadmPH.rst0 <- crop(gadmPH.rst0, sp::bbox(po))
# The mask the surface to the study area
gadmPH.rst <- mask(gadmPH.rst0, gadm)





# PLOT THE OUTPUT AND SAVE MAP --------------------------------------------

# Convert data to dataframe for plotting in ggplot
gadmPH.df <- data.frame(xyFromCell(gadmPH.rst,
                                   1:ncell(gadmPH.rst)), 
                        ph= values(gadmPH.rst))

# Define map extent parameters
area.lim2 <- st_bbox(gadm.sf)
min.x2 <- area.lim2["xmin"]
max.x2 <- area.lim2["xmax"]
min.y2 <- area.lim2["ymin"]
max.y2 <- area.lim2["ymax"]

# Create the map 
soilmap <- ggplot() + 
  geom_sf(data = gadm.sf,  colour="black", fill="ivory", size=0.2) + 
  geom_raster(data = gadmPH.df,  aes(x = x, y = y, fill=ph)) +
  scale_fill_gradientn(colours=c("red","yellow","blue"))  +
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, size = 0.5, 
                                    linetype = 'solid',
                                    colour = "black"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  ggsn::scalebar(x.min = min.x2, x.max = max.x2, 
                 y.min = min.y2, y.max = max.y2, 
                 dist = 10, dist_unit = "km", 
                 transform = TRUE, location = "bottomright", st.size = 3) +
  ggsn::north(x.min = min.x2, x.max = max.x2, 
              y.min = min.y2, y.max = max.y2, 
              scale = .08, symbol = 1) +
  labs( x="Longitude", y="Latitude")

# Save the map
ggsave("outputData/soilmap.jpeg", soilmap, 
       height = 21, width = 29.7, units = "cm", 
       limitsize = FALSE)

# CONNECT TO DATABASE AND SAVE SPATIAL OBJECT -----------------------------

# NB: Databese and user were created in the terminal - Applicants-remarks.txt
dbConnection <- dbConnect(RPostgres::Postgres(),
                          dbname = "newdb1",
                          host = "localhost", 
                          port = 5432,
                          user = "newuser1", 
                          password = {"newuser1_password"})


# Write the data and output into the database
# data

phData <- st_as_sf(po)
StudyArea <- st_as_sf(gadm)
st_write(phData, dbConnection, drop = TRUE)
st_write(StudyArea, dbConnection, drop = TRUE)

# output
interpolatedSurface <- st_as_sf(gadmPH)
st_write(interpolatedSurface, dbConnection, drop = TRUE)

dbDisconnect(dbConnection)
