#prepare a regression for wq=f(demographics)

library(here)
library(dplyr)
library(tidyverse)
library(car)
library(betareg)
library(lmtest)
library(stargazer)




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

data=merge(wq_window,demog,by=c("poi")) 

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

ggplot(data=data, aes(x=n)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of n")

#scatter exceedance and pct white
ggplot(data=data, aes(x=exceed100perc, y=white_pct)) + 
  geom_point()

ggplot(data=data, aes(x=exceed100perc, y=cfu2)) + 
  geom_point()

ggplot(data=data, aes(x=total, y=white_pct)) + 
  geom_point(alpha = 0.1)

ggplot(data=data, aes(x=exceed100perc, y=n)) + 
  geom_point()

ggplot(data=data, aes(x=exceed100perc, y=n)) + 
  geom_point(alpha = 0.5)

ggplot(data=data, aes(x=exceed100perc, y=total)) + 
  geom_point(alpha = 0.2)


ggplot(data = data, aes(x = exceed100perc, y = n)) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "exceed100perc", y = "n") +
  theme_bw()

# slice up data 

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
# first some simple, unweighted linear regressions
reg1=lm(exceed100perc~white_pct,data=data)
summary(reg1)
# significant race 

reg2=lm(exceed100perc~white_pct + med_household_income,data=data)
summary(reg2)
# significant race

reg3 = lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income,data=data)
summary(reg3)
# significant hispanic latino

reg4 = lm(exceed100perc~white_pct + black_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data)
summary(reg4)
# significant home value
vif(reg4)
#but VIFs are a bit high?

reg5 = lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data)
summary(reg5)
# significant home value?

reg1cfu = lm(cfu2~white_pct,data=data)
summary(reg1cfu)
# N.S.

reg2cfu = lm(cfu2~white_pct + black_pct + hispanic_or_latino_pct + med_household_income,data=data)
summary(reg2cfu)
# significant income 


#weighted

reg1w <- lm(exceed100perc ~ white_pct, data = data, weights = total)
summary(reg1w)
#significant race

reg2w <- lm(exceed100perc ~ med_household_income, data = data, weights = total)
summary(reg2w)
# N.S.income 

reg3w = lm(exceed100perc~white_pct + black_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data, weights = total)
summary(reg3w)
vif(reg3w)
# significant white and hispanic/latino
# concerning vifs 

reg4w = (lm(exceed100perc~white_pct + black_pct + asian_pct + two_or_more_races_pct + ntv_hw_pac_isl_pct + hispanic_or_latino_pct + am_ind_ak_ntv_pct + med_household_income + med_home_value,data=data, weights = total))
summary(reg4w)
vif(reg4w)
# wayyy too high VIFs (white is 50), probably not a valid model 

reg5w = lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data, weights = total)
summary(reg5w)
vif(reg5w)
# sig white and hispanic/latino 
# poor wq and pct hispanic/latino negatively correlated, interestingly 
# vifs fine 




#try other glms
# log linear? 
# random forest? 
betareg1 = 'something'

