# Load necessary spatial packages
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)

# Point location data import
# Import .CSV with GPS point data from Kruger's African Buffalo Dataset on movebank.org
# Import GPS data and store as object 'dGPS'
dGPS <- read.csv('D:/WILD 8980/Lab/Kruger_Buffalo_GPS.csv')

# Look at top 6 rows to ensure proper data import
head(dGPS)

#Select only the columns we want and store as a new data object called 'd'
d <- dGPS[, c('individual.local.identifier', 'timestamp', 'location.long', 'location.lat', 'sensor.type', 'external.temperature')]

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
# define the map center
# mapcenter <- c(31.6302944, -24.5201269)
# basic_map <- qmap(mapcenter, zoom=9, maptype = 'hybrid')

# Plot map with paths
# basic_map + geompath(aes(x = Long, y = Lat, col=ID), alpha=0.75, data = d)

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
d <- subset(d, (!(ID %in% NameDump_Dry) & Dry == 1)
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





