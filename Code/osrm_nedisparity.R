#OSRM testing 
library(osrm)
library(here)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(reshape2)


options(osrm.server = "http://0.0.0.0:5000/",osrm.profile = "driving")

locations = data.frame(comm_id = c("A", "B"),lon =c(-70.918836, -71.97875977) ,lat = c(42.55486, 41.727),measure="distance")

osrmTable(loc = locations[,2:3],measure=c("distance","duration"))

#what do we do for origins?
origins=read.csv("data/BGpoints_2010_latlon.csv")
destinations=read.csv("POIs_cut_points.csv")

#missing=read.csv("Missing_obs_v2.csv")

destinations=destinations[,c(3,1,2)]
origins=origins[,c(5,13,14)]

destinations=destinations[order(destinations$Unique_ID),]
 
origins=origins[order(origins$GEOID),]
origins = origins[!duplicated(origins[ ,"GEOID10"]), ] #duplicates in here for some reason

origins$folds=cut(seq(1,nrow(origins)),breaks=100,labels=FALSE)


distance=data.frame()
duration=data.frame()


ptm <- proc.time()

for (i in 1:100 ){
  or=origins[origins$folds==i,]
  b = osrmTable(src = or[1:3], dst = destinations, measure=c('duration', 'distance'))
  distance=rbind(distance,b$distances)
  duration=rbind(duration,b$durations)
  print(i)
}

proc.time() - ptm

save(distance,duration,file="OD_osrm_sonia_fix_raw.Rdata")

load("OD_osrm_sonia_fix_raw.Rdata")

distance$GEOID=row.names(distance)
duration$GEOID=row.names(duration)

distance_long= melt(distance, id.vars =c('GEOID'), value.name = "meters",variable.name="Unique_ID")
duration_long=melt(duration, id.vars =c('GEOID'), value.name = "min",variable.name="Unique_ID")

save(distance_long,duration_long,origins,destinations,file="OD_osrm_sonia_fix_parts.Rdata")

OD_long= merge(distance_long, duration_long, by=c("GEOID","Unique_ID"))

save(OD_long,origins,destinations,file="OD_osrm_sonia_fix2.Rdata")

write_csv(OD_long,"OD_osrm_sonia_fix2.csv")
