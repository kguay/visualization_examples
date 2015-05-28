# Title: hillshade_cluster_v1_3_hourly.R
# Author: Kevin Guay and Patrick Jantz
# Contact Info: kguay@whrc.org
# Purpose: Generate large area topographically distributed solar radiation
# for vegetation distribution modeling

# Description: This script implements the methodology outlined in 

# Pierce Jr, Kenneth B., Todd Lookingbill, and Dean Urban. "A simple method for estimating 
# potential relative radiation (PRR) for landscape-scale vegetation analysis." 
# Landscape Ecology 20.2 (2005): 137-147.

# This script iterates through digitial elevation models (organized by USGS NED map zones)
# and calculates topographically distributed relative solar radiation (RSR) values for each daylight
# hour for a "typical" solar day for each month. Typical solar days were identified using
# the University of Oregon Solar Radiation Monitoring Laboratory web application
# http://solardat.uoregon.edu/SunChartProgram.html

# To avoid inconsistencies introduced by latitude and longitude shifts, the calculations
# were carried out on 25 x 25km windows with ~12.5km overlaps. 

# The script calculates two layers for each window. 
# 1) Direct irradiance

# 2) Direct irradiance after applying topographic shading.

# The window size achieved reasonable processing speeds while ensuring that overlapping sections of rasters 
# differed by less than 0.01 in RSR values. Monthly estimates were summed to generate a yearly estimate
# of relative solar radiation. The 25 x 25 km yearly radiation rasters were then 
# mosaicked using a "minimum" operator to ensure that shaded areas
# would take precedence where windows overlapped. The 25 x 25 km window size places an 
# upper limit on shadow length but is an unavoidable consequence of the approach used 
# to minimize artifacts related to latitude and longitude shifts.

# Solar geometry and hillshade calculations were calculated using the R package "insol" which
# implements the equations described in

# Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters 
# from DEMs and the position of the sun for solar radiation modelling 
# in mountainous terrain, International Journal of Geographical Information Science 17(1), 1-23.

# This script requires the following output directories
# "Annual" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

########################################
# Add to library path and load libraries
#.libPaths(c(.libPaths(),"/home/pjantz/R/library"))

# Test run on map zone
t1 <- Sys.time()

require(raster)
require(insol)

# Set working directory and load matrix of indices and map zones
setwd("/nobackupnfs2/kguay/lccvp/")

load("/nobackup/kguay/lccvp/zones/wboxes_lat_lon.RData")

args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])
#i <- 888
print(paste('i=', i))

	
require(raster)
require(insol)
# Get vertical indices
vl <- as.numeric(wboxes[i,c(1:2)])
# Get horizontal indices
hl <- as.numeric(wboxes[i,c(3:4)])
# Get map zone
#mzone <- wboxes[i,5]

# check if the output directories exist
if (!file.exists(paste('out/', mzone, sep=''))){
	dir.create(file.path(paste('out/', mzone, '/Annual', sep='')), recursive = T)
}
	
# Set dem name
# setwd("C:/Share/LCC-VP/RangeWide/ned")
demname <- paste('/nobackup/kguay/lccvp/raster/', mzone,"_ned_alb_ecog.grd",sep="")

# Calculate julian days
l1 <- paste(c("2010/01/17","2010/02/14","2010/03/16","2010/04/15",
"2010/05/14","2010/06/07","2010/07/17",
"2010/08/17","2010/09/16","2010/10/16",
"2010/11/15","2010/12/11"),sep=" ")

# Expand to correspond with hours
l1 <- rep(l1,each=24)
# Convert to numeric
l1 <- lapply(l1, function(x) as.integer(unlist(strsplit(x,"/"))))  
# Make a list of times
l2 <- rep(0:23,12)

# Tack them together making a list of dates and times in the order
# year, month, day, hour
l3 <- mapply(function(x,y) c(x,y), l1,l2, SIMPLIFY=FALSE)

# Calculate julian day
jd <- lapply(l3, function(x) JDymd(x[1],x[2],x[3],x[4]))
# Time zone
atz <- as.numeric(wboxes[i,6])

# Function to iterate over julian day vector
# If the sun is not up, skip calculations
# x is the list of julian days
# y is the subsetted dem
# Function to iterate over julian day vector
# If the sun is not up, skip calculations
sunfun <- function(x) {
  
	sv <- sunvector(x,lat,lon,atz) # Calculate sun vector
	sp <- sunpos(sv) # Calculate azimuth and zenith
  
	# If zenith <= 90, it's daylight, run calculations
	# If not, skip calculations
	if (sp[,2] <= 90) {
    
		aras <- hillshading(cg,sv) # illuminance
		aras <- raster(aras)
		extent(aras) <- extent(s1)
		projection(aras) <- projection(s1)
    
		sh <- doshade(s1,sv) # shadows
		extent(sh) <- extent(s1)
		projection(sh) <- projection(s1)
        
		sh <- (aras*sh) # add shadows to hillshade
    
		# Get small box extents, clockwise from upper left
		rext <- extent(aras)
		# Specify offset
		ol1 <- 415
		box1 <- extent(c(rext@xmin+0,rext@xmin+(ol1*30),rext@ymax-(ol1*30),rext@ymax-0))
		box2 <- extent(c(rext@xmin+(ol1*30),rext@xmin+(ol1*2*30),rext@ymax-(ol1*30),rext@ymax-0))
		box3 <- extent(c(rext@xmin+(ol1*30),rext@xmin+(ol1*2*30),rext@ymax-(ol1*2*30),rext@ymax-(ol1*30)))
		box4 <- extent(c(rext@xmin+0,rext@xmin+(ol1*30),rext@ymax-(ol1*2*30),rext@ymax-(ol1*30)))
    
		outras1 <- crop(aras,box1)
		outras2 <- crop(aras,box2)
		outras3 <- crop(aras,box3)
		outras4 <- crop(aras,box4)
    
		sh1 <- crop(sh,box1)
		sh2 <- crop(sh,box2)
		sh3 <- crop(sh,box3)
		sh4 <- crop(sh,box4)

		return(list(outras1,outras2,outras3,outras4,sh1,sh2,sh3,sh4))
	}
}

#-------------------------------------------------------------------

# Calculate hillshade
# Requires a dem and bounding box indices
# Vertical and horizontal bounding box indices have 
# two components each, start and end cell numbers -relative to the bounding
# box of the map zone
# Also requires a julian day list
# The function works from left to right, then top to bottom

# Read in dem
print("Loading dem")
print(demname)
dem <- raster(demname, native=T)

# Subset the dem
# Get dem extent
exdem <- extent(dem)

# Calculate subset extent
cext <- extent(c(exdem@xmin+((hl[1]-1)*30),exdem@xmin+((hl[2])*30),exdem@ymax-((vl[2])*30) ,exdem@ymax-((vl[1]-1)*30)))
# Crop dem
s1 <- crop(dem, cext)
s1 <- s1/10

# If the dem subset is only NA values, skip
if (length(s1) != length(s1[is.na(s1)])) {
	
	# Extract coordinates
	lon <- as.numeric(wboxes[i,7])
	lat <- as.numeric(wboxes[i,8])
	
	
	# Returns a list of hillshade rasters for each daylight half hour for an "average"
	# solar day for each month
	cg <- cgrad(s1,30,30)
	hillshadelist <- lapply(jd, sunfun)
  
	# Get rid of null list elements in the
	# year, month, day, hour list
	l3nonull <- l3[!sapply(hillshadelist, is.null)]
	# Convert list to a matrix
	l3nonull <- do.call("rbind",l3nonull)
	# Sum number of daylight hours in each day
	dhf <- rle(paste(l3nonull[,1],"_",l3nonull[,2],"_",l3nonull[,3],sep=""))
	yrmoday <- do.call(rbind,strsplit(dhf[[2]],"_"))
	dhf <- data.frame(daylength=dhf[[1]],year=yrmoday[,1],month=yrmoday[,2],day=yrmoday[,3])
	# Convert daylength to indices 1-12
	print(paste('day length:', dhf$daylength))
	l3nonull <- rep(1:12,dhf$daylength)
  
	# Get rid of null list elements in the hillshade list
	hillshadelist <- hillshadelist[!sapply(hillshadelist, is.null)]

	# Stack monthly raster function
	stack4 <- function(y,hlist) {
		block <- stack(lapply(hlist, function(x) x[[y]]))
		return(block)
	}
  
	gc()

	##################
	# Irradiance stack
	irr <- lapply(as.list(1:4), stack4, hlist=hillshadelist)
	# Create monthly irradiances
	irrmonths <- lapply(as.list(1:4), function(x) stackApply(irr[[x]],indices=l3nonull,fun=sum,na.rm=TRUE))
	# Calculate annual irradiance
	irryear <- lapply(as.list(1:4), function(x) sum(irrmonths[[x]]))

	# Set outdir
	pre <- "/nobackupnfs2/kguay/lccvp/out/"

	# Set output annual irradiance name
	gname <- paste(mzone,"irry",as.character(as.integer(abs(lat*10000))),as.character(as.integer(abs(lon*10000))),sep="_")
	# But first write out day length file
	write.csv(dhf,paste(pre, mzone, '/', "Annual/",gname,".csv",sep=""))
	# Now set annual irradiance names for each quadrant
	gname <- lapply(as.list(1:4), function(x) paste(gname,"_","box",x,".asc",sep=""))

	# Write four rasters to disk. x is a list of integers from 1 to 4 and y is the name of a month giving
	# the subfolder to which the rasters will be written. z is the name of the list holding the rasters
	write4 <- function(x,y,z) {
		print(paste("Writing ", gname[[x]], " to disk.",sep=""))
		#writeRaster(z[[x]],filename=paste(pre, mzone, '/', y,"/",gname[[x]],sep=""), format='raster',  datatype='INT4S',overwrite=T)
		writeRaster(z[[x]],filename=paste(pre, mzone, '/', y,"/",gname[[x]],sep=""), format='ascii',overwrite=T)
	}

	# Write annual files to disk
	lapply(as.list(1:4), write4, y="Annual", z=irryear)

	# Write forty-eight (4 sets of 12) rasters to disk. x is a list of integers from 1 to 4 and y is the name of a month giving
	# the subfolder to which the rasters will be written. z is the name of the list holding the rasters. 
	# mo is the index corresponding to the month
	write48 <- function(x,moname,z,moindex) {  
		print(paste("Writing ", gname[[x]], " to disk.",sep=""))
		#writeRaster(z[[x]][[moindex]],filename=paste(pre, mzone, '/', moname,"/",gname[[x]],sep=""), format='raster',  datatype='INT4S',overwrite=T)
		writeRaster(z[[x]][[moindex]],filename=paste(pre, mzone, '/', moname,"/",gname[[x]],sep=""), format='ascii',overwrite=T)
	}

	# Set monthly irradiance grid name and save to disk
	# for (j in 1:12) {
	#
	#     # Set month names and then append box names to file name and path
	#     moname <- month.abb[j]
	#     gname <- paste(mzone,"irrm",as.character(j),as.character(as.integer(abs(lat*10000))),as.character(as.integer(abs(lon*10000))),sep="_")
	#     gname <- lapply(as.list(1:4), function(x) paste(gname,"_","box",x,".grd",sep=""))
	#
	#     # Save monthly irradiance
	#     lapply(as.list(1:4), write48, moname=moname, z=irrmonths, moindex=j)
	#   }
	gc()
	##################
	# Shadow stack
	sha <- lapply(as.list(5:8), stack4, hlist=hillshadelist)
	# Create monthly shadows
	shamonths <- lapply(as.list(1:4), function(x) stackApply(sha[[x]],indices=l3nonull,fun=sum,na.rm=TRUE))
	# Calculate annual shadows
	shayear <- lapply(as.list(1:4), function(x) sum(shamonths[[x]]))

	# Set output annual shadow name
	gname <- paste(mzone,"shay",as.character(as.integer(abs(lat*10000))),as.character(as.integer(abs(lon*10000))),sep="_")
	gname <- lapply(as.list(1:4), function(x) paste(gname,"_","box",x,".asc",sep=""))

	# Write annual files to disk
	lapply(as.list(1:4), write4, y="Annual", z=shayear)

	gc()
	# Set monthly shadow grid name and save to disk
	# for (j in 1:12) {
		#
		#     # Set month names and then append box names to file name and path
		#     moname <- month.abb[j]
		#     gname <- paste(mzone,"sham",as.character(j),as.character(as.integer(abs(lat*10000))),as.character(as.integer(abs(lon*10000))),sep="_")
		#     gname <- lapply(as.list(1:4), function(x) paste(gname,"_","box",x,".grd",sep=""))
		#
		#     # Save monthly hillshade
		#     lapply(as.list(1:4), write48, moname=moname, z=shamonths, moindex=j)
		#   }
	t2 <- Sys.time()
	print(t2-t1) # Get run time

} else {
	print("Tile contains NA values only, skipping.")
}
gc()


