# load all the things
library(FSA)
library(FSAdata)
library(plyr)
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(ggmap)
library(adehabitatHR)
library(lubridate)

# load PSDlit and store as data.frame for easy reference
PSDlit_df <- data.frame(PSDlit)
PSDlit_df
head(PSDlit_df)

# set wd and load data
setwd('C:/WILD 8980/Clean_Flathead_Data')
d <- read.csv('./Flathead_MMR_Master_Fixed.csv')


# append d with psd categorical vector
d$PSD <- psdAdd(TL.cm~species, data=d, units='cm', use.names = TRUE, verbose = TRUE)
head(d)

# save copy of df with new vector as .CSV
write.csv(d, file='Flathead_MMR_PSDs.csv')

# re-store date as POSIX object
#df$date <- mdy(df$date)

#class(df$date)

#table(df$PSD)

# create new boolean variable for each PSD class
# substock
d$substock <- if_else(d$PSD == 'substock', 1, 0)

# stock
d$stock <- if_else(d$PSD == 'stock', 1, 0)

# quality
d$quality <- if_else(d$PSD == 'quality', 1, 0)

# preferred
d$preferred <- if_else(d$PSD == 'preferred', 1, 0)

# memorable
d$memorable <- if_else(d$PSD == 'memorable', 1, 0)

# trophy
d$trophy <- if_else(d$PSD == 'trophy', 1, 0)

# save another copy as .csv
write.csv(d, file='Flathead_MMR_Boolean_PSDs.csv')

# convert to spatial points object
coordinates(d) <- d[, c('lon', 'lat')]

# assign CRS to SPDF
proj4string(d) <- CRS('+proj=longlat +datum=WGS84')

# transform from lat/lon to UTMs
dUTM <- spTransform(d, CRSobj = CRS('+proj=utm +zone=36 +datum=WGS84'))

#### subsetting by size class ####

# PSD counts
# substock
countSubstock <- table(d$substock)
countSubstock

# stock
countStock <- table(d$stock)
countStock

# quality
countQuality <- table(d$quality)
countQuality

# preferred
countPreferred <- table(d$preferred)
countPreferred

# memorable
countMemorable <- table(d$memorable)
countMemorable

# trophy
countTrophy <- table(d$trophy)
countTrophy

# subset data by substock & refactor
dSubstock <- subset(dUTM, substock == 1, select = 'fish.id')
dSubstock$fish.id <- factor(dSubstock$fish.id)

# subset data by stock
dStock <- subset(dUTM, stock == 1, select = 'fish.id')
dStock$fish.id <- factor(dStock$fish.id)

# subset data by quality
dQuality <- subset(dUTM, quality == 1, select = 'fish.id')
dQuality$fish.id <- factor(dQuality$fish.id)

# subset data by preferred
dPreferred <- subset(dUTM, preferred == 1, select = 'fish.id')
dPreferred$fish.id <- factor(dPreferred$fish.id)

# subset data by memorable
dMemorable <- subset(dUTM, memorable == 1, select = 'fish.id')
dMemorable$fish.id <- factor(dMemorable$fish.id)

# subset data by trophy
dTrophy <- subset(dUTM, trophy == 1, select = 'fish.id')
dTrophy$fish.id <- factor(dTrophy$fish.id)

# import river polygon as boundary
bound <- readOGR('C:/Users/jwyea/OneDrive/Documents/GIS Projects/Online DB files/Satilla_River_from_Michael/Satilla_Stuff/Satilla_area.shp')
bound@proj4string
class(bound)

# transform bound to UTMs
boundUTM <- spTransform(bound, CRS('+proj=utm +zone=36 +datum=WGS84'))

# transform boundUTM from polygon to line
boundUTM_line <- as(boundUTM, 'SpatialLines')
class(boundUTM_line)

# entire dataset
allHR <- kernelUD(dUTM, h='href', kern = c(bivnorm), grid = 200,
                       extent = 1.75, boundary = boundUTM_line)
