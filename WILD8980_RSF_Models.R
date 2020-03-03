# Load necessary spatial packages
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)

# Point location data import
# Import .CSV with GPS point data from Kruger's African Buffalo Dataset on movebank.org
# Import GPS data and store as object 'dGPS'
dGPS <- read.csv('F:/WILD8980/Lab/Data/Data/Data/KrugerBuffalo_GPS_Crossetal2009_MB1764627.csv')

# Look at top 6 rows to ensure proper data import
head(dGPS)

#Select only the columns we want and store as a new data object called 'd'
d <- dGPS[, c('individual.local.identifier', 'timestamp', 'location.long', 'location.lat', 'sensor.type', 'comments')]

# rename these columns
names(d) <- c('ID', 'DateTime', 'Long', 'Lat', 'Dtype', 'Temp')

# Another look and the top of the data file
head(d)

# Check for any duplicated records moving from beginning to end
any(duplicated(d, fromLast=F))

## [1] FALSE

# and from the end to the beginning
any(duplicated(d, fromLast=T))

# import the lubridate package
library(lubridate)

# Re-store DateTime as POSIX object
d$DateTime <- ymd_hms(d$DateTime, tz='GMT')

# Identify the class structure of new DateTime variable
class(d$DateTime)

## [1] "POSIXct" "POSIXt"
d$Month <- month(d$DateTime)
d$Year <- year(d$DateTime)

# visualizing point data
library(ggmap)

# NEED GOOGLE MAPS API?
register_google(key = "AIzaSyAK5maO_gQp1vB6ZejmtzfLfxWVrEvuIKE", write = TRUE)

# define the map center
mapcenter <- c(31.6302944, -24.5201269)
basic_map <- qmap(mapcenter, zoom=9, maptype = 'hybrid')

# Plot map with paths
basic_map + geompath(aes(x = Long, y = Lat, col=ID), alpha=0.75, data = d)

#########################################################
## 3rd Order: Generating home ranges, considering time ##
#########################################################

# Identify unique individuals
NumIndividuals <- unique(d$ID)
NumIndividuals

# count # of unique individuals
length(NumIndividuals)

# Frequency table, identifies the number of Locations by ID and year
table(d$ID, d$Year)

# Creates a new variable called 'dry' (1 = Dry, 0 = wet)
d$Dry <- ifelse(d$Month >= 6 & d$Month <= 11, 1, 0)

# stores a count table by ID and season (dry)
CountBySeason <- table(d$ID, d$Dry)
CountBySeason

# identifies the row names (animal IDs) in CountBySeason with fewer than 20 fixes
NameDump_Wet <- row.names(CountBySeason)[which(CountBySeason[, 1] < 20)]
NameDump_Dry <- row.names(CountBySeason)[which(CountBySeason[, 2] < 20)]

# display those names that have fewer than 20 fixes
NameDump_Wet
NameDump_Dry

#subset by removing those individuals
#d <- subset(d, (!(ID %in% NameDump_Dry) & Dry == 1)
            #(!(ID %in% NameDump_Wet) & Dry == 0))
d <- subset(d, (!(ID %in% NameDump_Dry) & Dry == 1) |
              (!(ID %in% NameDump_Wet) & Dry == 0))

CountBySeason <- table(d$ID, d$Dry)
CountBySeason

# converting to a spatial points object

# create a SpatialPointsDataFrame by simply defining for that data.frame
coordinates(d) <- d[, c('Long', 'Lat')]

# assign a CRS object to the SPDF
proj4string(d) <- CRS('+proj=longlat +datum=WGS84')

# transform from lat/long to UTMs
dUTM <- spTransform(d, CRSobj = CRS('+proj=utm +zone=36 +datum=WGS84'))

# loade the adehabitatHR pkg
library(adehabitatHR)

# subset the data by season
dDry <- subset(dUTM, Dry == 1, select='ID')

# re-factor to remove IDs with 0 recs
dDry$ID <- factor(dDry$ID)

# generate home ranges by ID and Dry season
dryHR <- kernelUD(dDry, h = 'href', kern = c('bivnorm'),
                  grid = 200, extent = 1.75)

# same as above for wet season
dWet <- subset(dUTM, Dry == 0, select='ID')
dWet$ID <- factor(dWet$ID)
wetHR <- kernelUD(dWet, h = 'href', kern = c('bivnorm'),
                  grid = 200, extent = 1.5)

# display the raster for a single animal
image(dryHR[['Cilla']])

# convert the estUD to a form that persp() can interpret
cilla <- as.image.SpatialGridDataFrame(dryHR[['Cilla']])

# 3D perspective plot showing UD
persp(cilla, theta = -0.5,phi = 40, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.25,
      xlab = "Easting", ylab = "Northing", zlab = "Intensity of Use")

## calculation of the 95% home range
polyDry <- getverticeshr(dryHR, percent = 95, unin = 'm', unout = 'km2')
polyWet <- getverticeshr(wetHR, percent = 95, unin = 'm', unout = 'km2')

## convert SpatialPolygonsDF back to lat/long
pD <- spTransform(polyDry, CRS('+proj=longlat +datum=WGS84'))
pW <- spTransform(polyWet, CRS('+proj=longlat +datum=WGS84'))

## converts SpatialPolygonsDF to a data.frame
polyDryDF <- fortify(pD)

mapcenter <- c(31.6302944, -24.5201269)
basic_map <- qmap(mapcenter, zoom = 9, maptype = "hybrid")
basic_map + geom_polygon(aes(x = long, y = lat, fill=id), alpha=0.75,
                         data = polyDryDF)

## only one home range
basic_map + geom_polygon(aes(x = long, y = lat, fill=id), alpha=0.75,
                         data = polyDryDF[polyDryDF$id == 'Cilla',])

######################
# Exporting polygons #
######################

# create a new directory for storing files if one doesn't exist
dir <- './Buffalo_HRs'
dir.create(dir, showWarnings = F)

# write vector shape files for home ranges
# note the absence of a .shp extension!
writeOGR(pD, dir, 'DrySeason_HRs_Iso95', driver="ESRI Shapefile")
writeOGR(pW, dir, 'WetSeason_HRs_Iso95', driver="ESRI Shapefile")

########################################################
# Generate random points - Sytematic vs. Random Points #
########################################################

## 3rd order selection availability
# systematic points...taking every 10th 30m pixel
cellsize <- 30

for (p in 1:length(polyDry)) {
  sys_points <- spsample(SpatialPolygons(polyDry@polygons[p]),
                         cellsize = cellsize,
                         type = 'nonaligned', pretty=F)
  
  ID <- rep(polyDry@data$id[p], length(sys_points))
  Dry <- rep(1, length(sys_points))
  
  sys_points <- SpatialPointsDataFrame(sys_points,
                                       data = data.frame(ID = ID, Dry = Dry))
  sys_points@proj4string <- polyDry@proj4string
  
  if (p == 1) sysAvailPoints <- sys_points
  
}

for (p in 1:length(polyWet)) {
  sys_points <- spsample(SpatialPolygons(polyWet@polygons[p]),
                         cellsize = cellsize,
                         type = 'nonaligned', pretty=F)
  
  ID <- rep(polyWet@data$id[p], length(sys_points))
  Dry <- rep(0, length(sys_points))
  
  sys_points <- SpatialPointsDataFrame(sys_points,
                                       data = data.frame(ID = ID, Dry = Dry))
  sys_points@proj4string <- polyWet@proj4string
  
  sysAvailPoints <- rbind(sysAvailPoints, sys_points)
}

######################
# Simplified version #
######################

source('F:/WILD8980/Lab/Exercises/Exercises/Day1_HandlingDataInR/Source/spatialSample.R')

# generating random points by season
# 'stratified' is stratified random within a cell
# multiplier is relative to number of used points

# by dry season
dryCounts <- CountBySeason[, 2]
randAvailDry10x <- spsample2(polyDry, type = 'stratified',
                             counts = dryCounts, multiplier = 10, pretty=F)
randAvailDry10x@data$Dry <- 1    # adds a new dummy variable for Dry season

# by wet season
wetCounts <- CountBySeason[, 1]
randAvailWet10x <- spsample2(polyWet, type = 'stratified',
                             counts = wetCounts, multiplier = 10, pretty=F)
randAvailWet10x@data$Dry <- 0

#####################################
# Check diff. in sampling intensity #
#####################################

