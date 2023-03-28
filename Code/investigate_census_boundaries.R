# Created by Erin Burman, 3/27/2023
# look at block group boundary differences between 2010 and 2020 census 

library(here)
library(dplyr)
library(tidyverse)
library(tidycensus)


rm(list=ls()) #clear all

####Create POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

setwd(wd)

census=read.csv("Census_blockgroup_comparison_2010_2020_RI.csv")

# make dataframe of bgs that don't match between years 
# should get count of splits and merges
census_changes = census %>%
  filter(GEOID_BLKGRP_20 != GEOID_BLKGRP_10
  )

census_changes = census %>%
  filter(OID_BLKGRP_20 != OID_BLKGRP_10
  )