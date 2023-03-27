###########################################################################################
##
## R source code to read and visualize Köppen-Geiger fields (Version of 29 August 2017)                                                                                    
##
## Climate classification after Kottek et al. (2006), downscaling after Rubel et al. (2017)
##
## Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006: World Map of the  
## Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.
##
## Rubel, F., K. Brugger, K. Haslinger, and I. Auer, 2017: The climate of the 
## European Alps: Shift of very high resolution Köppen-Geiger climate zones 1800-2100. 
## Meteorol. Z., DOI 10.1127/metz/2016/0816.
##
## (C) Climate Change & Infectious Diseases Group, Institute for Veterinary Public Health
##     Vetmeduni Vienna, Austria
##
###########################################################################################

# required packages 
library(raster); library(rasterVis); library(rworldxtra); data(countriesHigh)
library(rgdal)
library(maptools)
library(maps)

# Read raster files
period='1986-2010'
r <- raster(paste('/Users/haydnthomas/ShrubHub/ShrubHub/scripts/users/hthomas/Main Text/Introduction/KG_', period, '.grd', sep=''))
x1=-180; x2=180; y1=40; y2=90
r <- crop(r, extent(x1, x2, y1, y2))
newproj <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
r_polar <- projectRaster(r, crs=newproj)

Dat <- read.table("Koeppen-Geiger-ASCII.txt", header = TRUE)
head(Dat)


# Color palette for climate classification
climate.colors=c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "#8fbbbd", "#5f9ea0", "#8fbbbd", "#5f9ea0", "white", "#8fbbbd", "#5f9ea0","grey50", "cornflowerblue", "white", "white", "white", "white", "white", "white")

wrld <- map(plot=FALSE, interior=FALSE, wrap=TRUE, ylim=c(40, 90), xlim=c(-180, 180), col="black",lwd = 1.5)
wrld_sp <- map2SpatialLines(wrld)
proj4string(wrld_sp) <- CRS("+proj=longlat")
laea_wrld_sp <- spTransform(wrld_sp, CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"))

r_polar$layer

pdf(file="scripts/users/hthomas/Main Text/Introduction/tundra_map.pdf",width=4,height=4)
image(r_polar, col = climate.colors, xaxt='n', yaxt='n', ylab = "", xlab = "", maxpixels=1e8)
plot(laea_wrld_sp, add = T, lwd = 0.25)
dev.off()

