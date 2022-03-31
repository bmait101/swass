

# calculate # of musky waters within x distance from a given lake
library(sf)
library(ggplot2)
library(maps)

#pull in necessary raw data
setwd("C:/Users/latzka/Documents/Fisheries/Musky/muskellunge waters data/")
lakes = read.csv("musky_lakes_complete_2012.csv",header=T)
rivs = read.csv("Musky River Classes by County.csv",header=T)

all.wbics = unique(c(lakes$WBIC,rivs$WBIC))

#strings to pass to get_dnr_layer
all.wbics.lines = paste0("RIVER_SYS_WBIC IN (",paste0("'",all.wbics,"'",collapse=","),")")
all.wbics.wb = paste0("WATERBODY_WBIC IN (",paste0("'",all.wbics,"'",collapse=","),")")
line.wbic.list=function(wbics){
  string = paste0("RIVER_SYS_WBIC IN (",paste0("'",wbics,"'",collapse=","),")")
}
wb.wbic.list=function(wbics){
  string = paste0("WATERBODY_WBIC IN (",paste0("'",wbics,"'",collapse=","),")")
}

#pull in hydro layer lakes and rivers using WBICs from musky waters data
library(wdnr.gis)

hydro_rivs = get_dnr_layer(
  url = list_urls(layers = "24K Hydrography Streams and Rivers"),
  where = all.wbics.lines
)
hydro_rivs = st_transform(hydro_rivs,crs=3071)

hydro_wbs = get_dnr_layer(
  url=list_urls(layers="24K Hydrography Lakes and Open Water"),
  where=all.wbics.wb
)
hydro_wbs=st_transform(hydro_wbs,crs=3071)

#3 lakes missing from these spatial features - none of them are actually in Hydro
missing = all.wbics[!all.wbics %in% unique(c(hydro_wbs$WATERBODY_WBIC,hydro_rivs$RIVER_SYS_WBIC))]
lakes[lakes$WBIC %in% missing,]

#now get lat lon for a particular lake
my.wbic = 805400 # mendota
my.wbic = 2331600 #trout

this.lake = get_dnr_layer(
  url=list_urls(layers="24K Hydrography Lakes and Open Water"),
  where=wb.wbic.list(my.wbic))
this.lake = st_transform(this.lake,crs=3071) #uses wisconsin transverse mercator

center = st_centroid(this.lake$geoms)
buff = st_buffer(center,miles_to_meters(60))

nearby.wbs = hydro_wbs[st_is_within_distance(center,hydro_wbs,miles_to_meters(60))[[1]],]
nearby.lines = hydro_rivs[st_is_within_distance(center,hydro_rivs,miles_to_meters(60))[[1]],]

nearby.wbics = unique(c(nearby.wbs$WATERBODY_WBIC,nearby.lines$RIVER_SYS_WBIC))
nearby.lakes = lakes[lakes$WBIC %in% nearby.wbics,]
nearby.rivs = rivs[rivs$WBIC %in% nearby.wbics,]

wisc = ne_states(geounit="wisconsin",returnclass="sf")
wisc = st_as_sf(map("county",plot=FALSE,fill=TRUE))
wisc = subset(wisc, grepl("wisconsin",wisc$ID))

ggplot(nearby.wbs) +
  geom_sf(fill="white") +
  geom_sf(data=this.lake,inherit.aes=FALSE) +
  #geom_sf(fill="blue") +
  geom_sf(buff)
  
ggplot(buff) +
  geom_sf() +
  geom_sf(nearby.wbs)
 
ggmap()
ggplot(nearby.wbs) +
  geom_sf() +
  geom_sf(data=buff, fill=NA, color="black") +
  geom_sf(data=this.lake, fill="blue",color="blue") + 
  geom_sf(data=center)

ggplot(wisc) +
  geom_sf() +
  geom_sf(data=nearby.wbs,fill="gray70",color="gray50") +
  geom_sf(data=buff, fill=NA, color="black") +
  geom_sf(data=this.lake, fill="blue",color="blue") + 
  #geom_sf(data=center) +
  ggtitle("Trout Lake, Vilas County")

ggplot(data=world) +
  geom_sf()


#now run this for all lakes and calculate some summary metrics and create some maps/dataframes for some
#high profile lakes
highprof = c()

lake.list = read.csv("C:/Users/latzka/Documents/Fisheries/Shiny/Lake Class/data/report_on_fm_lakechar_ref.csv",
                     header=T,
                     na.strings=c("-",""," "))
lake.list = lake.list[which(lake.list$Lake.Impment.Acres.Amt>=100),]
lake.list = lake.list[order(lake.list$County.Name,lake.list$Wbic.Name),]


musky.opportunities = data.frame(WBIC=lake.list$Wbic.Code,Waterbody=lake.list$Wbic.Name,County=lake.list$County.Name)

pdf()

for (i in (1:nrow(lake.list))){
#for (i in (1:40)){
  #pull in data & objects
  wbic = lake.list$Wbic.Code[i]
  this.lake = get_dnr_layer(
    url=list_urls(layers="24K Hydrography Lakes and Open Water"),
    where=wb.wbic.list(wbic))
  this.lake = st_transform(this.lake,crs=3071) #uses wisconsin transverse mercator
  
  
  
  #if this lake is not in hydro, use lat lon from musky data or lake reference table
  if (nrow(this.lake)==0){
    center = st_as_sf(lake.list[i,],coords=c("Ll.Cntr.Lon.Dd.Amt","Ll.Cntr.Lat.Dd.Amt"),crs=4326)
    center = st_transform(center,crs=3071)
  } else {
    center = st_centroid(this.lake$geoms)
  }
  
#  for (d in c(30,45,60)){
    buff30 = st_buffer(center,miles_to_meters(30))
    nearby.wbs.30 = hydro_wbs[st_is_within_distance(center,hydro_wbs,miles_to_meters(30))[[1]],]
    nearby.lines.30 = hydro_rivs[st_is_within_distance(center,hydro_rivs,miles_to_meters(30))[[1]],]
    nearby.wbics.30 = unique(c(nearby.wbs.30$WATERBODY_WBIC,nearby.lines.30$RIVER_SYS_WBIC))
    nearby.lakes.30 = lakes[lakes$WBIC %in% nearby.wbics.30,]
    nearby.rivs.30 = rivs[rivs$WBIC %in% nearby.wbics.30,]
    
#  }
    buff45 = st_buffer(center,miles_to_meters(45))
    nearby.wbs.45 = hydro_wbs[st_is_within_distance(center,hydro_wbs,miles_to_meters(45))[[1]],]
    nearby.lines.45 = hydro_rivs[st_is_within_distance(center,hydro_rivs,miles_to_meters(45))[[1]],]
    nearby.wbics.45 = unique(c(nearby.wbs.45$WATERBODY_WBIC,nearby.lines.45$RIVER_SYS_WBIC))
    nearby.lakes.45 = lakes[lakes$WBIC %in% nearby.wbics.45,]
    nearby.rivs.45 = rivs[rivs$WBIC %in% nearby.wbics.45,]
    
    buff60 = st_buffer(center,miles_to_meters(60))
    nearby.wbs.60 = hydro_wbs[st_is_within_distance(center,hydro_wbs,miles_to_meters(60))[[1]],]
    nearby.lines.60 = hydro_rivs[st_is_within_distance(center,hydro_rivs,miles_to_meters(60))[[1]],]
    nearby.wbics.60 = unique(c(nearby.wbs.60$WATERBODY_WBIC,nearby.lines.60$RIVER_SYS_WBIC))
    nearby.lakes.60 = lakes[lakes$WBIC %in% nearby.wbics.60,]
    nearby.rivs.60 = rivs[rivs$WBIC %in% nearby.wbics.60,]
    
    #for average driving distance of 30 miles = 1.41*30 = 42.5 miles

    buff42.5 = st_buffer(center,miles_to_meters(42.5))
    nearby.wbs.42.5 = hydro_wbs[st_is_within_distance(center,hydro_wbs,miles_to_meters(42.5))[[1]],]
    nearby.lines.42.5 = hydro_rivs[st_is_within_distance(center,hydro_rivs,miles_to_meters(42.5))[[1]],]
    nearby.wbics.42.5 = unique(c(nearby.wbs.42.5$WATERBODY_WBIC,nearby.lines.42.5$RIVER_SYS_WBIC))
    nearby.lakes.42.5 = lakes[lakes$WBIC %in% nearby.wbics.42.5,]
    nearby.rivs.42.5 = rivs[rivs$WBIC %in% nearby.wbics.42.5,]
  
  
  #calculate relevant metrics
  
  #make a map in the pdf
  
  #output data
  musky.opportunities$MuskyWaters60miles[i]=length(unique(nearby.wbics.60))
  musky.opportunities$MuskyWaters45miles[i]=length(unique(nearby.wbics.45))
  musky.opportunities$MuskyWaters30miles[i]=length(unique(nearby.wbics.30))
  musky.opportunities$MuskyWaters42.5miles[i]=length(unique(nearby.wbics.42.5))
  musky.opportunities$MuskyLakeAcreage60miles[i]=sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.60])
  musky.opportunities$MuskyLakeAcreage45miles[i]=sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.45])
  musky.opportunities$MuskyLakeAcreage30miles[i]=sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.30])
  musky.opportunities$MuskyLakeAcreage42.5miles[i]=sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.42.5])
  musky.opportunities$A1MuskyWaters60miles[i] = length(unique(nearby.wbics.60[nearby.wbics.60 %in% lakes$WBIC[lakes$Class=="A1"]]))
  musky.opportunities$A1MuskyWaters45miles[i]= length(unique(nearby.wbics.45[nearby.wbics.45 %in% lakes$WBIC[lakes$Class=="A1"]]))
  musky.opportunities$A1MuskyWaters30miles[i]= length(unique(nearby.wbics.30[nearby.wbics.30 %in% lakes$WBIC[lakes$Class=="A1"]]))
  musky.opportunities$A1MuskyWaters42.5miles[i]= length(unique(nearby.wbics.42.5[nearby.wbics.42.5 %in% lakes$WBIC[lakes$Class=="A1"]]))
  
  musky.opportunities$A2MuskyWaters60miles[i] = length(unique(nearby.wbics.60[nearby.wbics.60 %in% lakes$WBIC[lakes$Class=="A2"]]))
  musky.opportunities$A2MuskyWaters45miles[i]= length(unique(nearby.wbics.45[nearby.wbics.45 %in% lakes$WBIC[lakes$Class=="A2"]]))
  musky.opportunities$A2MuskyWaters30miles[i]= length(unique(nearby.wbics.30[nearby.wbics.30 %in% lakes$WBIC[lakes$Class=="A2"]]))
  musky.opportunities$A2MuskyWaters42.5miles[i]= length(unique(nearby.wbics.42.5[nearby.wbics.42.5 %in% lakes$WBIC[lakes$Class=="A2"]]))
  
  musky.opportunities$BMuskyWaters60miles[i] = length(unique(nearby.wbics.60[nearby.wbics.60 %in% lakes$WBIC[lakes$Class=="B"]]))
  musky.opportunities$BMuskyWaters45miles[i]= length(unique(nearby.wbics.45[nearby.wbics.45 %in% lakes$WBIC[lakes$Class=="B"]]))
  musky.opportunities$BMuskyWaters30miles[i]= length(unique(nearby.wbics.30[nearby.wbics.30 %in% lakes$WBIC[lakes$Class=="B"]]))
  musky.opportunities$BMuskyWaters42.5miles[i]= length(unique(nearby.wbics.42.5[nearby.wbics.42.5 %in% lakes$WBIC[lakes$Class=="B"]]))
  
  musky.opportunities$CMuskyWaters60miles[i] = length(unique(nearby.wbics.60[nearby.wbics.60 %in% lakes$WBIC[lakes$Class=="C"]]))
  musky.opportunities$CMuskyWaters45miles[i]= length(unique(nearby.wbics.45[nearby.wbics.45 %in% lakes$WBIC[lakes$Class=="C"]]))
  musky.opportunities$CMuskyWaters30miles[i]= length(unique(nearby.wbics.30[nearby.wbics.30 %in% lakes$WBIC[lakes$Class=="C"]]))
  musky.opportunities$CMuskyWaters42.5miles[i]= length(unique(nearby.wbics.42.5[nearby.wbics.42.5 %in% lakes$WBIC[lakes$Class=="C"]]))
  
  musky.opportunities$A1MuskyAcreage60miles[i] = sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.60 & lakes$Class=="A1"])
  musky.opportunities$A1MuskyAcreage45miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.45 & lakes$Class=="A1"])
  musky.opportunities$A1MuskyAcreage30miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.30 & lakes$Class=="A1"])
  musky.opportunities$A1MuskyAcreage42.5miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.42.5 & lakes$Class=="A1"])
  
  musky.opportunities$A2MuskyAcreage60miles[i] = sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.60 & lakes$Class=="A2"])
  musky.opportunities$A2MuskyAcreage45miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.45 & lakes$Class=="A2"])
  musky.opportunities$A2MuskyAcreage30miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.30 & lakes$Class=="A2"])
  musky.opportunities$A2MuskyAcreage42.5miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.42.5 & lakes$Class=="A2"])

  musky.opportunities$BMuskyAcreage60miles[i] = sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.60 & lakes$Class=="B"])
  musky.opportunities$BMuskyAcreage45miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.45 & lakes$Class=="B"])
  musky.opportunities$BMuskyAcreage30miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.30 & lakes$Class=="B"])
  musky.opportunities$BMuskyAcreage42.5miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.42.5 & lakes$Class=="B"])
  
  musky.opportunities$CMuskyAcreage60miles[i] = sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.60 & lakes$Class=="C"])
  musky.opportunities$CMuskyAcreage45miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.45 & lakes$Class=="C"])
  musky.opportunities$CMuskyAcreage30miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.30 & lakes$Class=="C"])
  musky.opportunities$CMuskyAcreage42.5miles[i]= sum(lakes$Acres[lakes$WBIC %in% nearby.wbics.42.5 & lakes$Class=="C"])
  
}


#all.lakes=get_fmdb_site_ref()

write.csv(musky.opportunities,"Musky waters near all lakes.csv",row.names = F)

png("Musky waters within 42.5 miles.png",width=6,height=8,units="in",res=300)
par(mar=c(3,3,0.7,0.1),oma=c(0.1,0.1,3,0.4),mgp=c(2,0.5,0),las=1,tcl=-0.25,xpd=NA)
layout(matrix(c(1,1,2,2,3,4,5,6),nrow=4,byrow=T))
hist(musky.opportunities$MuskyWaters42.5miles,breaks=20,
     xlab="Cumulative Number of Musky Waters",
     ylab="Frequency",
     las=1,main="",col="lightblue")
box()
hist(musky.opportunities$MuskyLakeAcreage42.5miles,breaks=20,
     xlab="Cumulative Musky lake acreage",
     ylab="Frequency",
     las=1,main="",col="lightblue")
box()
hist(musky.opportunities$A1MuskyWaters42.5miles,breaks=20,
     xlab="Number of Class A1 Musky Lakes",
     ylab="Frequency",
     las=1,main="",col="lightblue")
box()
hist(musky.opportunities$A2MuskyWaters42.5miles,breaks=20,
     xlab="Number of Class A2 Musky Lakes",
     ylab="Frequency",
     las=1,main="",col="lightblue")
box()
hist(musky.opportunities$BMuskyWaters42.5miles,breaks=20,
     xlab="Number of Class B Musky Lakes",
     ylab="Frequency",
     las=1,main="",col="lightblue")
box()
hist(musky.opportunities$CMuskyWaters42.5miles,breaks=20,
     xlab="Number of Class C Musky Lakes",
     ylab="Frequency",
     las=1,main="",col="lightblue")
box()
mtext("Musky waters within 42.5 miles (~30 road miles)\nof each 100 acre lake in Wisconsin",outer=T)
dev.off()
