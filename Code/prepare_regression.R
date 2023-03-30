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

#bring in beach demographics cal it dem3 if you dont edit the rest of this

#connect wq to beaches window
wq_window=read.csv("data/bacteria_window.csv")

data2=merge(wq_window,dem3,by=c("poi")) 

#regress wq on demographics
reg2=lm(exceed100perc~white,data=data2)
summary(reg2)



