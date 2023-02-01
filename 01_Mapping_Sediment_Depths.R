############################################################
# MAPPING SEDIMENT DEPTHS 
############################################################
# Holgerson Lab, Katie Gannon
# Code modified from Adam Heathcote 

# KG R environment set up 


# 0. Set up R environment and Load Data 

  #Set working directory: 
    setwd("~/Sediment_Mapping_DEC") # Desktop 
    getwd()
    
  # Load packages: 
    # Standard 
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

    # Heathcote Packages 
      library(shapefiles)
      library(maptools)
      library(sp)
      library(rgdal)
      library(geoR)
      library(gstat)
      library(maps)

    #read in Shape Files from Adam 
        getwd()
        getinfo.shape("Heathcote_Adam_Code/Data_files_for_Meredith/Data_Files_for_Meredith_notmac/torangeauNEW.shp")
        getinfo.shape("Heathcote_Adam_Code/Data_files_for_Meredith/Data_Files_for_Meredith_notmac/tourangeaupoly.shp")
        
        #convert to ShaePoly object
        lake.points.heath <- readShapePoints("Heathcote_Adam_Code/Data_files_for_Meredith/Data_Files_for_Meredith_notmac/torangeauNEW.shp")
        lake.poly.heath <- readShapePoly("Heathcote_Adam_Code/Data_files_for_Meredith/Data_Files_for_Meredith_notmac/tourangeaupoly.shp")
        
    # Read in Shape Files for DEC sediment Mapping (made by KG in GIS)
        setwd("~/Sediment_Mapping_DEC")
        getinfo.shape("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_with_Edge_020123_1407.shp")
        getinfo.shape("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp")
        
        lake.points.hol <- readShapePoints("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_with_Edge_020123_1407.shp")
        lake.poly.hol <- readShapePoly("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp")

    

# 1. Create a Custom functions for scalebar and north arrow 
        
  # 1.1 Custom function for a scalebar 
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

  # 1.2 Custom function for a north arrow
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
    
# 2. plots of measured locations
    
    #Heathcote 
      spplot(lake.points.heath, "z")
      bubble(lake.points.heath, "z", maxsize=2, pch=1, col=1, main="Observed Lake Depth")
        
    # Holgerson
      spplot(lake.points.hol, "Sediment_T")
      bubble(lake.points.hol, "Sediment_T", maxsize=2, pch=1, col=1, main="Observed Sediment Depth")

# 3. Create grid
          
  # Heathcote Data 
      grid.lav.heath <- polygrid(xgrid=seq(min(lake.points.heath@coords[,1]), max(lake.points.heath@coords[,1]), 10), ygrid=seq(min(lake.points.heath@coords[,2]), max(lake.points.heath@coords[,2]), 10), borders=lake.poly.heath, vec.inout = FALSE)
      plot(grid.lav.heath, pch=19)
      plot(lake.poly.heath)
      
      #convert to gridded spatial object
      grid.sp.heath <- na.omit(grid.lav.heath)
      coordinates(grid.sp.heath) <-c("x", "y")
      gridded(grid.sp.heath) <- T
      
      #plot grid
      plot(grid.sp.heath, col="black")
      
  # Holgerson Date 
      grid.lav.hol <- polygrid(xgrid=seq(min(lake.points.hol@coords[,1]), max(lake.points.hol@coords[,1]), 0.00001), ygrid=seq(min(lake.points.hol@coords[,2]), max(lake.points.hol@coords[,2]), 0.00001), borders=lake.poly.hol, vec.inout = FALSE)
      plot(grid.lav.hol, pch=19)
      plot(lake.poly.hol)
      
      #convert to gridded spatial object
      grid.sp.hol <- na.omit(grid.lav.hol)
      coordinates(grid.sp.hol) <-c("x", "y")
      gridded(grid.sp.hol) <- T
      
      #plot grid
      plot(grid.sp.hol, col="black")
      

# 4. create variogram with Cressie-Hawkins estimator
      
      # Heathcote 
        lav.vc.heath <- variogram(z ~1, lake.points.heath, cressie=T)
        plot(lav.vc.heath, main = "Variogram Heathcote Data")
        
      # Holgerson 
        names(lake.points.hol@data)[names(lake.points.hol@data) == "Sediment_T"] <- "z"  # change the column name for sediment thickness to z so that this function can find it 
        lav.vc.hol <- variogram(z ~1, lake.points.hol, cressie=T)
        plot(lav.vc.hol, main = "Variogram Holgerson Data")   # need to change the names to make sediment thickness names z 
        
  
# KG Has it working to here! 020123  <--------------------------   
                    
#5. fit spherical, exponential, Whittle, and Matern k=1.9 (pseudo-normal) functions to variogram 
        
  # Heathcote 
  dep.Sph.heath <- fit.variogram(lav.vc.heath, vgm(psill=6, range=600, model="Sph", nugget=0.8))
  dep.Exp.heath <- fit.variogram(lav.vc.heath, vgm(psill=6, range=600, model="Exp", nugget=0.8))
  dep.Whi.heath <- fit.variogram(lav.vc.heath, vgm(psill=6, range=600, model="Mat", nugget=0.8, kappa=1))
  dep.Mat.heath <- fit.variogram(lav.vc.heath, vgm(psill=6, range=600, model="Mat", nugget=0.8, kappa=1.9))
  
  # Holgerson   
    # Giving errors that make me think that we don't have enough data to run these kinds of models --> take a look at bathymetry code from Nick
  dep.Sph.hol <- fit.variogram(lav.vc.hol, vgm(psill=6, range=600, model="Sph", nugget=0.8))
  dep.Exp.hol <- fit.variogram(lav.vc.hol, vgm(psill=6, range=600, model="Exp", nugget=0.8))
  dep.Whi.hol <- fit.variogram(lav.vc.hol, vgm(psill=6, range=600, model="Mat", nugget=0.8, kappa=1))
  dep.Mat.hol <- fit.variogram(lav.vc.hol, vgm(psill=6, range=600, model="Mat", nugget=0.8, kappa=1.9))

# 6. plot various models onto of empirical variogram - models for kringing (I think - KG 1/19)
  
  # Heathcote 
  par(mar=c(5,5,3,1))
  plot(lav.vc.heath$dist, lav.vc.heath$gamma, ylim=c(0, max(lav.vc.heath$gamma)), pch=19, lwd.ticks=4, xlab="h", ylab=expression(paste(gamma)), cex.lab=2, font=2, cex=1.5, main="Semi-variogram for Z with cutoff of 800")
  box(lwd=4)
  lines(variogramLine(dep.Sph.heath, 800), lwd=4)
  lines(variogramLine(dep.Exp.heath, 800), col=2, lwd=4)
  lines(variogramLine(dep.Whi.heath, 800), col=3, lwd=4)
  lines(variogramLine(dep.Mat.heath, 800), col=4, lwd=4)
  legend("bottomright", lwd=4, col=c(1:4), legend=c("Sph", "Exp", "Whi", "Mat"), bty="n", cex=2)
  
  # Holgerson 
  # No point in doing this step on our little ponds because there is only the one model that works for us 
      par(mar=c(5,5,3,1))
      plot(lav.vc.hol$dist, lav.vc.hol$gamma, ylim=c(0, max(lav.vc.hol$gamma)), pch=19, lwd.ticks=4, xlab="h", ylab=expression(paste(gamma)), cex.lab=2, font=2, cex=1.5, main="Semi-variogram for Z with cutoff of 800")
      box(lwd=4)
      lines(variogramLine(dep.Sph.hol, 800), lwd=4)
      lines(variogramLine(dep.Exp.hol, 800), col=2, lwd=4)
      lines(variogramLine(dep.Whi.hol, 800), col=3, lwd=4)
      lines(variogramLine(dep.Mat.hol, 800), col=4, lwd=4)
      legend("bottomright", lwd=4, col=c(1:4), legend=c("Sph", "Exp", "Whi", "Mat"), bty="n", cex=2)
      

# 7. quantify sum of squared error for each model
      
  # Heathcote 
  attr(dep.Sph.heath, "SSErr")
  attr(dep.Exp.heath, "SSErr")
  attr(dep.Whi.heath, "SSErr")
  attr(dep.Mat.heath, "SSErr")
  
  # Holgerson 
  attr(dep.Sph.hol, "SSErr")
  attr(dep.Exp.hol, "SSErr")
  attr(dep.Whi.hol, "SSErr")
  attr(dep.Mat.hol, "SSErr")

  #print variogram model parameters
    dep.Sph.heath
    dep.Whi.heath
    
    dep.Sph.hol
    dep.Whi.hol

# 8. use ordinary kriging with constant mean and Whittle model (smallest error)
  tour.k.heath <- krige(z ~ 1, lake.points.heath, grid.sp.heath, dep.Whi.heath)
  tour.k.hol <- krige(z ~ 1, lake.points.hol, grid.sp.hol, dep.Sph.hol)

# 9. replace kriged estimated of < 0 with very small positive value (0.001)
  tour.k.heath@data$var1.pred[tour.k.heath@data$var1.pred < 0] <- 0.001
  tour.k.hol@data$var1.pred[tour.k.hol@data$var1.pred < 0] <- 0.001

# 10. create custom color ramp
    cols <- colorRampPalette(colors=c("cadetblue1", "navyblue"))
    contours <- c(seq(0, 2, 0.2))
    
# 11. Plot depth 
  spplot(tour.k.heath, "var1.pred", col.regions=cols, contour=F, par.settings = list(axis.line = list(col =  'transparent')))
  spplot(tour.k.hol, "var1.pred", col.regions=cols, contour=F, par.settings = list(axis.line = list(col =  'transparent')))
    # this model just pulls everything to the mean -- try other modeling methods 
    mean(lake.points.hol@data$z)
  

#variance plot (as standard deviations)
tour.k@data$var1.sd <- sqrt(tour.k@data$var1.var)
spplot(tour.k, "var1.sd", par.settings = list(axis.line = list(col =  'transparent')))

#save as ESRI compatible rasterfile 
#writeGDAL(tour.k["var1.pred"], "TourKrigingPred.asc")


#set contour interval
contours <- c(seq(0, max(tour.k@data$var1.pred), 0.5))

#contour plot
plot(lake.poly) 
  plot(lake.points)
contour(tour.k["var1.pred"], col=cols(length(contours)),levels=contours, add=T)
axis(side=1)
axis(side=2)
box()
scalebar(loc=c(332100, 5312300), length=500, unit="m")
northarrow(loc=c(334000, 5312250), size=100, cex=0.0001)

#contours alone
contour(tour.k["var1.pred"], col=1,add=F)



