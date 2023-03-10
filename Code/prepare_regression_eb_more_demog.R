#prepare a regression for wq=f(demographics)

library(here)
library(dplyr)
library(tidyverse)
library(tidycensus)


rm(list=ls()) #clear all

####Create POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
#wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

#setwd(wd)

trips=read.csv("data/trips_by_blockgroup_yearly_NewEngland_oct22.csv")

#bring in block group demographics
census_api_key("aa5b0c4e8e217972b3b77d48c78ba980a2a5c0cd", install=TRUE,overwrite=TRUE)
readRenviron("~/.Renviron")

statelist=c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
#statelist=c('AL','AK')

trips$GEOID=trips$Home


allvarlist <- load_variables(2018, "acs5", cache = TRUE)

varlist=c("B02001_001",	"B02001_002")
varname=c("Estimate!!Total","Estimate!!Total!!White alone")

dem_m=data.frame()
for (state in statelist) {

  if (state=="AL"){
    dem <- get_acs(geography = "block group", 
                   variables = varlist,
                   state=state,
                   year = 2018)
    
    dem=spread(dem, variable, estimate)
    
    dem= dem %>%
      group_by(GEOID) %>%
      summarise_each(funs(mean(., na.rm = TRUE)))
    
    drops <- c("NAME","moe")
    dem=dem[ , !(names(dem) %in% drops)]
  

    dem_m=merge(trips,dem,by="GEOID")
    
  } else {
    
    dem <- get_acs(geography = "block group", 
                   variables = varlist,
                   state=state,
                   year = 2018)
    
    dem=spread(dem, variable, estimate)
    
    dem= dem %>%
      group_by(GEOID) %>%
      summarise_each(funs(mean(., na.rm = TRUE)))
    
    drops <- c("NAME","moe")
    dem=dem[ , !(names(dem) %in% drops)]
    
    dem2=merge(trips,dem,by="GEOID")
    
    dem_m=rbind(dem_m,dem2)
  }
}

# process and fix census vars
dem_m=dem_m[dem_m$B02001_001>0,]
dem_m$white=dem_m$B02001_002/dem_m$B02001_001 #create %white

save(dem_m,file="data/block_demographics.Rdata")
#load(file="data/block_demographics.Rdata")

dem=dem_m

#dem=dem[dem$trips>0,]

#"trip-weight"/apportion census vars
dem$whitetrips=dem$trips*dem$white

#collapse demographics to beaches

dem2= dem %>%
  group_by(Poi,year) %>%
  summarise(total=sum(trips),
            whitetrips=sum(whitetrips),
            white= whitetrips/total)

#connect wq to beaches
wq=read.csv("data/bacteria_yearly.csv")
dem2$poi=dem2$Poi
data=merge(wq,dem2,by=c("poi"))

#regress wq on demographics
reg1=lm(exceed100perc~white,data=data)
summary(reg1)

