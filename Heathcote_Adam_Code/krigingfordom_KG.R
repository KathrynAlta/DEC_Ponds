# Code from Adam Heathcote 

# KG R environment set up 
    # Set working directory 
    setwd("~/Sediment_Mapping_DEC/Heathcote_Adam_Code") # Desktop 
    getwd()
    
    # Load standard packages 
    library(dplyr)
    library(raster)
    library(ape) 
    library(mosaic)
    library(readxl)
    library(tidyverse)
    library(devtools)
    library(lme4)
    library(car)
    library(effects)
    library(mosaic)
    library(writexl)
    library(purrr)
    library(EnvStats)
    # Spatial packages 
    library(sf)   # classes and functions for vector data
    library(ggspatial)
    library(terra)   # classes and functions for raster data
    library(spData) # load geographic data
    library(spDataLarge)  # load larger geographic data
    
    
    # Install Packages that Adam Uses 
    #   install.packages("shapefiles")
    #   install.packages("maptools")
    #   install.packages("rgdal")
    #   install.packages("geoR")
    #   install.packages("geoR")
    #   install.packages("gstat")
    #   install.packages("maps")
   # install.packages("GDAL")


# Start Adam Set up 
# setwd("/Volumes/GoogleDrive/My Drive/For Tonya/Lac Tourangeau/Files for Meredith")
      
library(shapefiles)
library(maptools)
library(sp)
library(rgdal)
library(geoR)
library(gstat)
library(maps)

#read in Lac Tourangeau polygon shapefile
    getwd()
    getinfo.shape("Spatial_Data/torangeauNEW.shp")
    getinfo.shape("Spatial_Data/tourangeaupoly.shp")
    
    #convert to ShaePoly object
    st_read
    lake.points <- st_read("Spatial_Data/torangeauNEW.shp")
    lake.poly <- st_read("Spatial_Data/tourangeaupoly.shp")

#custom function for a scalebar 
  scalebar <- function(loc,length,unit="km",division.cex=.8,...) {
    if(missing(loc)) stop("loc is missing")
    if(missing(length)) stop("length is missing")
    x <- c(0,length/c(4,2,4/3,1),length*1.1)+loc[1]
    y <- c(0,length/(10*3:1))+loc[2]
    cols <- rep(c("black","white"),2)
    for (i in 1:4) rect(x[i],y[1],x[i+1],y[2],col=cols[i])
    for (i in 1:5) segments(x[i],y[2],x[i],y[3])
    labels <- x[c(1,3)]-loc[1]
    labels <- append(labels,paste(x[5]-loc[1],unit))
    text(x[c(1,3,5)],y[4],labels=labels,adj=.5,cex=division.cex)
  }

#custom function for a north arrow
    northarrow <- function(loc,size,bearing=0,cols,cex=1,...) {
        
      # checking arguments
        if(missing(loc)) stop("loc is missing")
        if(missing(size)) stop("size is missing")
        # default colors are white and black
        if(missing(cols)) cols <- rep(c("white","black"),8)
        
        # calculating coordinates of polygons
        radii <- rep(size/c(1,4,2,4),4)
        x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
        y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
        
        # drawing polygons
        for (i in 1:15) {
            x1 <- c(x[i],x[i+1],loc[1])
            y1 <- c(y[i],y[i+1],loc[2])
            polygon(x1,y1,col=cols[i])
          }
        
        # drawing the last polygon
        polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
       
        # drawing letters
        b <- c("E","N","W","S")
        for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
        (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
        cex=cex)
    }

# **************************************************************************************
    # Here is where we run into problems, I think that it is because I ended up bringin in the shape files using st_read instead of "readShapePoints"
    #    and "readShapePoly" that Adam used (I couldn't get them to play nice with my version of R) and so the formatting is off 
    #    - KG 010923 
    
    
    
#plots of measured locations
spplot(lake.points, "z")
plot(lake.points, "z")
bubble(lake.points, "z", maxsize=2, pch=1, col=1, main="Observed Lake Depth")

#create grid
grid.lav <- polygrid(xgrid=seq(min(lake.points@coords[,1]), max(lake.points@coords[,1]), 10), ygrid=seq(min(lake.points@coords[,2]), max(lake.points@coords[,2]), 10), borders=lake.poly, vec.inout = FALSE)
plot(grid.lav, pch=19)

#convert to gridded spatial object
grid.sp <- na.omit(grid.lav)
coordinates(grid.sp) <-c("x", "y")
gridded(grid.sp) <- T

#plot grid
plot(grid.sp, col="black")

#create variogram with Cressie-Hawkins estimator
lav.vc <- variogram(z ~1, lake.points, cressie=T)
plot(lav.vc)


#fit spherical, exponential, Whittle, and Matern k=1.9 (pseudo-normal) functions to variogram 
dep.Sph <- fit.variogram(lav.vc, vgm(psill=6, range=600, model="Sph", nugget=0.8))
dep.Exp <- fit.variogram(lav.vc, vgm(psill=6, range=600, model="Exp", nugget=0.8))
dep.Whi <- fit.variogram(lav.vc, vgm(psill=6, range=600, model="Mat", nugget=0.8, kappa=1))
dep.Mat <- fit.variogram(lav.vc, vgm(psill=6, range=600, model="Mat", nugget=0.8, kappa=1.9))

#plot various models onto of empirical variogram
par(mar=c(5,5,3,1))
plot(lav.vc$dist, lav.vc$gamma, ylim=c(0, max(lav.vc$gamma)), pch=19, lwd.ticks=4, xlab="h", ylab=expression(paste(gamma)), cex.lab=2, font=2, cex=1.5, main="Semi-variogram for Z with cutoff of 800")
box(lwd=4)
lines(variogramLine(dep.Sph, 800), lwd=4)
lines(variogramLine(dep.Exp, 800), col=2, lwd=4)
lines(variogramLine(dep.Whi, 800), col=3, lwd=4)
lines(variogramLine(dep.Mat, 800), col=4, lwd=4)
legend("bottomright", lwd=4, col=c(1:4), legend=c("Sph", "Exp", "Whi", "Mat"), bty="n", cex=2)

#quantify sum of squared error for each model
attr(dep.Sph, "SSErr")
attr(dep.Exp, "SSErr")
attr(dep.Whi, "SSErr")
attr(dep.Mat, "SSErr")

#print variogram model parameters
dep.Sph
dep.Whi

#use ordinary kriging with constant mean and Whittle model (smallest error)
tour.k <- krige(z ~ 1, lake.points, grid.sp, dep.Whi)

#replace kriged estimated of < 0 with very small positive value (0.001)
tour.k@data$var1.pred[tour.k@data$var1.pred < 0] <- 0.001

#create custom color ramp
cols <- colorRampPalette(colors=c("cadetblue1", "navyblue"))
contours <- c(seq(0, 2, 0.2))

spplot(tour.k, "var1.pred", col.regions=cols, contour=F, par.settings = list(axis.line = list(col =  'transparent')))

#variance plot (as standard deviations)
tour.k@data$var1.sd <- sqrt(tour.k@data$var1.var)
spplot(tour.k, "var1.sd", par.settings = list(axis.line = list(col =  'transparent')))

#save as ESRI compatible rasterfile 
#writeGDAL(tour.k["var1.pred"], "TourKrigingPred.asc")


#set contour interval
contours <- c(seq(0, max(tour.k@data$var1.pred), 0.5))

#contour plot
plot(lake.poly) 
contour(tour.k["var1.pred"], col=cols(length(contours)),levels=contours, add=T)
axis(side=1)
axis(side=2)
box()
scalebar(loc=c(332100, 5312300), length=500, unit="m")
northarrow(loc=c(334000, 5312250), size=100, cex=0.0001)

#contours alone
contour(tour.k["var1.pred"], col=1,add=F)
