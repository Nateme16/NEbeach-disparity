#prepare a regression for wq=f(demographics)

library(here)
library(dplyr)
library(tidyverse)
library(car)
library(betareg)
library(lmtest)




rm(list=ls()) #clear all

####Create POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

setwd(wd)

#bring in beach demographics cal it dem3 if you dont edit the rest of this
demog = get(load("beach_demographics.Rdata"))
#rename Poi to poi so it merges
demog = rename(demog, poi = Poi)

#connect wq to beaches window
wq_window=read.csv("bacteria_window.csv")

# important vars you might use 
'
poi    
cfu2                          
exceed100
exceed100perc

total

white_pct                    
black_pct   
am_ind_ak_ntv_pct
asian_pct                     
ntv_hw_pac_isl_pct        
some_other_race_pct          
two_or_more_races_pct        
two_races_including_pct     
two_races_excluding_pct

hispanic_or_latino_pct     
not_h_white_pct                    
not_h_black_pct                      
not_h_ai_an_pct             
not_h_asian_pct   

med_household_income              
med_home_value
'

#exploratory data analysis 
head(data)
summary(data)

ggplot(data=data, aes(x=exceed100perc)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of exceedance percents")

ggplot(data=data, aes(x=cfu2)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of cfus")

ggplot(data=data, aes(x=white_pct)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of pctwhite")

#scatter exceedance and pct white
ggplot(data=data, aes(x=exceed100perc, y=white_pct)) + 
  geom_point()


# look for nulls 
sapply(data, function(x) sum(is.na(x)))
#none, though I feel like there should be some?


round(cor(data[c('white_pct',                    
                 'black_pct',   
                 'am_ind_ak_ntv_pct',
                 'asian_pct',                   
                 'ntv_hw_pac_isl_pct',        
                 'some_other_race_pct',        
                 'two_or_more_races_pct',        
                 'two_races_including_pct',     
                 'two_races_excluding_pct',
                 
                 'hispanic_or_latino_pct',     
                 'not_h_white_pct',                    
                 'not_h_black_pct',                      
                 'not_h_ai_an_pct',             
                 'not_h_asian_pct',   
                 
                 'med_household_income',              
                 'med_home_value' )]), 2)


#regress wq on demographics
reg1=lm(exceed100perc~white_pct,data=data)
summary(reg1)
# significant race 

reg1cfu = lm(cfu2~white_pct,data=data)
summary(reg1cfu)
# N.S.

reg2=lm(exceed100perc~white_pct + med_household_income,data=data)
summary(reg2)
# significant race

reg3 = lm(exceed100perc~white_pct + black_pct + hispanic_or_latino_pct + med_household_income,data=data)
summary(reg3)
# N.S. everything 
vif(reg3)
#but VIFs are a bit high?

reg4 = lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data)
summary(reg4)
# significant home value?

#try other glms
betareg1 = 'something'

