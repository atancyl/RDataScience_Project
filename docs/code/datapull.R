#Data Pulling
#TIF Raster Data used:
#NOAA/NGDC - Earth Observation Group - 
#Defense Meteorological Satellite Progam, Boulder.
#Available at: https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites.html
#Downloaded for month September year 2014 and month September year 2016
#Data Used in this project: Global Cloud Free VIIRS Cloud Mask product is selected (VCMCFG);
#Renamed file to "092014avg_rade9.tif" and "092014avg_rade9.tif".
#Saved at local "[path]/img" directory for local processing

#Preliminary Libraries
library(rgdal)

#Set TIF Raster images
###############################################################################################################
#Assigning TIF files to their respective years' variables

### Year 2014 TIF
rast2014=raster(paste("img/092014avg_rade9.tif",sep=""))

### Year 2016 TIF
rast2016=raster(paste("img/092016avg_rade9.tif",sep=""))

#Census Vector Data
#Using Census Core Based Statistical Areas Shape Files Data at 1:20,000,000 resolution
#Datum spec is WGS 84
#Downloaded for year 2014 and 2016
#https://www.census.gov/geo/maps-data/data/cbf/cbf_msa.html

##Population estimate for year 2014 and 2016 downloaded at:
#https://www2.census.gov/programs-surveys/popest/datasets/

#Set Census Data
###############################################################################################################
#Assigning MSA Shapefiles to respective years' variables
msa2014=readOGR("censusdata/cb_2014_us_cbsa_20m.shp")
msa2016=readOGR("censusdata/cb_2016_us_cbsa_20m.shp")

#Assign WGS84 as the datum projection variable
wgs84="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Using WGS84 as datum projection with census shape files
#As detailed in:
#https://books.google.com/books?isbn=0128047933
projection(msa2014)=CRS(wgs84)
projection(msa2016)=CRS(wgs84)

###############################################################################################################
#Format population estimate files' values from numbers to characters for streamlining readability
msa_pop2014=read.csv("censusdata/cbsa-est2014-alldata.csv")
msa_pop2014=msa_pop2014[msa_pop2014$LSAD=="Metropolitan Statistical Area",]
msa_pop2014=msa_pop2014[order(msa_pop2014$POPESTIMATE2014),]
msa_pop2014$NAME=as.character(msa_pop2014$NAME)

msa_pop2016=read.csv("censusdata/cbsa-est2016-alldata.csv")
msa_pop2016=msa_pop2016[msa_pop2016$LSAD=="Metropolitan Statistical Area",]
msa_pop2016=msa_pop2016[order(msa_pop2016$POPESTIMATE2016),]
msa_pop2016$NAME=as.character(msa_pop2016$NAME)

