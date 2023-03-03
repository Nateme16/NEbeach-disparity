#prepare a regression for wq=f(demographics)

library(here)
library(dplyr)
library(tidyverse)
library(tidycensus)


rm(list=ls()) #clear all

####Create POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

setwd(wd)

trips=read.csv("trips_by_blockgroup_yearly_NewEngland_oct22.csv")

#bring in block group demographics
census_api_key("6f1fcfb97bd029c14c188870d6e118942b993a60")

statelist=c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
#statelist=c('AL','AK')

trips$GEOID=trips$Home

dem_m=data.frame()

for (state in statelist) {

  if (state=="AL"){
    dem <- get_acs(geography = "block group", 
                   variables = c(medincome = "B19013_001"),
                   state=state,
                   year = 2018)
    
    dem_m=merge(trips,dem,by="GEOID")
    
  } else {
    
    dem <- get_acs(geography = "block group", 
                   variables = c(medincome = "B19013_001"),
                   state=state,
                   year = 2018)
    
    dem2=merge(trips,dem,by="GEOID")
    dem_m=rbind(dem_m,dem2)
  }
  
}

dem_m$income=dem_m$estimate

#collapse demographics to beaches

#connect wq to beaches

#regress wq on demographics

