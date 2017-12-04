#Load libraries
{
  library(doParallel)
  library(foreach)
  library(raster)
  library(sp)
  library(rgdal)
  library(ggmap)
  library(ggplot2)
}
#####################################################################################

#Data
source("code/datapull.R")
#####################################################################################

#Selecting Cities
cities=c("Chicago, IL", "Detroit, MI", "Flint, MI", "Cleveland, OH", "Buffalo, NY")

##Chart Display Parameters The  th
par(mai=c(0,0,0,0),mfrow = c(3,2),bg='#001a4d', bty='n')
######################################################################################

#Visualizing Cities for Year 2016
coords2016=data.frame()

##Loop
for(i in 1:length(cities)){
  
  ##Coords
  temp_coord=geocode(override_limit = TRUE, cities[i], source = c("google", "dsk"), key = "AIzaSyBRzBKmnnK9vwKDrqSBR6olexFThQNWU-8")
  coords2016=rbind(coords2016,temp_coord)
  
  #Bounding Box
  boundbox=extent(temp_coord$lon - 1, temp_coord$lon + 1,
              temp_coord$lat - 0.25, temp_coord$lat + 0.25)
  
  #Pasting Boundbox to Raster
  rc=crop(rast2016, boundbox)    
  
  ##Rescale brackets
  sampled=as.vector(rc)
  clusters=15
  clust=kmeans(sampled,clusters)$cluster
  combined=as.data.frame(cbind(sampled,clust))
  brk=sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
  
  #Plots
  plot(rc, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters), 
       legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
  text(temp_coord$lon ,temp_coord$lat + 0.15,
       substr(cities[i],1,regexpr(",",cities[i])-1), 
       col="white", cex=1.25)
  
  rm(combined)
}

#Saving Geocodes  
write.csv(coords2016,file ="data/coords2016.csv" )
#########################################################################################

#Visualizing Cities for Year 2014
########################################################################################
coords2014=data.frame()

##Loop
for(i in 1:length(cities)){
  
  ##Coords
  temp_coord=geocode(override_limit = TRUE, cities[i], source = c("google", "dsk"), key = "AIzaSyBlAs3JB7y49JPpUWMQvZGdie2ZqWNmZss")
  coords2014=rbind(coords2014,temp_coord)
  
  #Bounding Box
  boundbox=extent(temp_coord$lon - 1, temp_coord$lon + 1,
                  temp_coord$lat - 0.25, temp_coord$lat + 0.25)
  
  #Pasting Boundbox to Raster
  rc=crop(rast2014, boundbox)    
  
  ##Rescale brackets
  sampled=as.vector(rc)
  clusters=15
  clust=kmeans(sampled,clusters)$cluster
  combined=as.data.frame(cbind(sampled,clust))
  brk=sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
  
  #Plots
  plot(rc, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters), 
       legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
  text(temp_coord$lon ,temp_coord$lat + 0.15,
       substr(cities[i],1,regexpr(",",cities[i])-1), 
       col="white", cex=1.25)
  
  rm(combined)
}

#Saving Geocodes
write.csv(coords2014,file ="data/coords2014.csv" )
###############################################################################################

#Creating Image Histogram of Remote Sensing Image (Raster Data to Vector Data Conversion)
##Histogram 2016
histogram=function(shp,rast2016,i){
  
  #Extract one polygon based on index value i
  polygon=shp[i,] #extract one polygon
  extent=extent(polygon) #extract the polygon extent 
  
  #Raster extract
  outer=crop(rast2016, extent) #extract raster by polygon extent
  inner=mask(outer,polygon) #keeps values from raster extract that are within polygon
  
  #Convert cropped raster into a vector
  #Specify coordinates
  coords2016=expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                        seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  
  #Convert raster into vector
  data=as.vector(inner)
  
  #tidying data into dataframe
  data=cbind(as.character(shp@data$CBSAFP[i]),coords2016, data) 
  colnames(data)=c("GEOID","lon","lat","avg_rad") #note that 
  data=data[!is.na(data$avg_rad),] #keep non-NA values only
  
  return(data)
}

##Histogram 2014
histogram=function(shp,rast2014,i){
  
  #Extract one polygon based on index value i
  polygon=shp[i,] #extract one polygon
  extent=extent(polygon) #extract the polygon extent 
  
  #Raster extract
  outer=crop(rast2014, extent) #extract raster by polygon extent
  inner=mask(outer,polygon) #keeps values from raster extract that are within polygon
  
  #Convert cropped raster into a vector
  #Specify coordinates
  coords2014=expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                         seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  #Convert raster into vector
  data=as.vector(inner)
  
  #package data in neat dataframe
  data=cbind(as.character(shp@data$CBSAFP[i]),coords2014, data) 
  colnames(data)=c("GEOID","lon","lat","avg_rad") #note that 
  data=data[!is.na(data$avg_rad),] #keep non-NA values only
  
  return(data)
}

#################################################################################################
#Plotting
#Scatterplot with Trendline of "TNL and Population"

##Year 2016
registerDoParallel(cores=2)
extract2016=foreach(i=1:nrow(msa2016@data), .combine=rbind, .packages="raster") %dopar% {
  data=histogram(msa2016,rast2016,i)
  data.frame(GEOID = data$GEOID[1],sum = sum(data$avg_rad))
}
extract2016$GEOID=as.numeric(as.character(extract2016$GEOID))

##Merge data
merge2016=merge(extract2016, msa_pop2016[,c("CBSA","NAME","POPESTIMATE2016")],by.x="GEOID",by.y="CBSA")

colnames(merge2016)=c("GEOID","TNL","MSA","Population")

#Saving Large Extracted Data 2016
write.csv(extract2016,file ="data/extract2016.csv" )

#Plotting
ggplot(merge2016, aes(log(TNL), log(Population))) +
  geom_hex() +
  geom_smooth(aes(color = log(TNL))) +
  labs(title="Total Night Time Light vs Population 2016")
    

#---------------------------------------------------------------------------------------------------
##Year 2014
registerDoParallel(cores=2)
extract2014=foreach(i=1:nrow(msa2014@data), .combine=rbind, .packages="raster") %dopar% {
  data=histogram(msa2014,rast2014,i)
  data.frame(GEOID = data$GEOID[1],sum = sum(data$avg_rad))
}
extract2014$GEOID=as.numeric(as.character(extract2014$GEOID))

#Saving Large Extracted Data 2014  
write.csv(extract2014,file ="data/extract2014.csv" )

##Merge data
merge2014=merge(extract2014, msa_pop2014[,c("CBSA","NAME","POPESTIMATE2014")],by.x="GEOID",by.y="CBSA")

colnames(merge2014)=c("GEOID","TNL","MSA","Population")

#Plotting
ggplot(merge2014, aes(log(TNL), log(Population))) +
  geom_hex() +
  geom_smooth(aes(color = log(TNL))) +
  labs(title="Total Night Time Light vs Population 2014")


