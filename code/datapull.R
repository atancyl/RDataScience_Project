#Data
#Data reference in study available at:
# TIF Images: URL 1
# Census Shapefile: URL 2
# MSA names: URL 3

#Original Data
# Original Source for Census Data: Census URL
# Original Source for TIF Images: NOAA URL

#Preliminary Libraries
library(rgdal)

#Set TIF images
##################################################################################################################
##Obtain a list of TIF files, load in the first file in list

### Year 2014 TIF
rast2014=raster(paste("img/092014avg_rade9.tif",sep=""))

### Year 2016 TIF
rast2016=raster(paste("img/092016avg_rade9.tif",sep=""))
  
##Specify WGS84 as the projection of the raster file
wgs84="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(rast2014)=CRS(wgs84)
projection(rast2016)=CRS(wgs84)
###################################################################################################################

#Set Census Data
####################################################################################################################
##Setting MSA Shapefile
msa2014=readOGR("censusdata/cb_2014_us_cbsa_20m.shp")
msa2016=readOGR("censusdata/cb_2016_us_cbsa_20m.shp")

##Synchronizing projection with TIF Files
projection(msa2014)=CRS(wgs84)
projection(msa2016)=CRS(wgs84)

##Convert MSA names to char
msa_pop2014=read.csv("censusdata/cbsa-est2014-alldata.csv")
msa_pop2014=msa_pop2014[msa_pop2014$LSAD=="Metropolitan Statistical Area",]
msa_pop2014=msa_pop2014[order(msa_pop2014$POPESTIMATE2014),]
msa_pop2014$NAME=as.character(msa_pop2014$NAME)

msa_pop2016=read.csv("censusdata/cbsa-est2016-alldata.csv")
msa_pop2016=msa_pop2016[msa_pop2016$LSAD=="Metropolitan Statistical Area",]
msa_pop2016=msa_pop2016[order(msa_pop2016$POPESTIMATE2016),]
msa_pop2016$NAME=as.character(msa_pop2016$NAME)

