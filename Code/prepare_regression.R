#prepare a regression for wq=f(demographics)

library(here)
library(pscl)
library(boot)
library(dplyr)
library(tidyverse)
library(car)
library(lmtest)
library(stargazer)
library(AER)
library(MASS)
library(broom)
library(randomForest)
library(margins)



rm(list=ls()) #clear all

####Create Airsage beaches POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

setwd(wd)

#bring in beach demographics
demog = get(load("beach_demographics.Rdata"))
#rename Poi to poi so it merges
demog = rename(demog, poi = Poi)

#connect wq to beaches window
wq_window=read.csv("bacteria_window.csv")

data=merge(wq_window,demog,by=c("poi")) 

# remove unsupported null types
data[is.na(data) | data=="Inf"] = NA

#make median household income in thousands so coefficients 
# are easier to interpret 
data$med_household_income_k = data$med_household_income / 1000

#make pct exceedances out of 100 
data$exceed100perc_x100 = data$exceed100perc * 100

'
  _____ ____    _     
 | ____|  _ \  / \    
 |  _| | | | |/ _ \   
 | |___| |_| / ___ \  
 |_____|____/_/   \_\ 
                      
  Exploratory Data Analysis                      

'
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

ggplot(data=data, aes(x=exceed100)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of exceedance numbers")

ggplot(data=data, aes(x=exceed100perc_x100)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of exceedance percents")

#logged exceedences looks more normal 
ggplot(data=data, aes(x=log(exceed100perc))) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of logged exceedance percents")

ggplot(data=data, aes(x=cfuGeomMean)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of cfus")

#logged cfu also looks better
ggplot(data=data, aes(x=log(cfuGeomMean))) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of logged cfus")

ggplot(data=data, aes(x=white_pct)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of pctwhite")

ggplot(data=data, aes(x=n)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of n")

#scatter exceedance and pct white
ggplot(data=data, aes(x=exceed100perc, y=white_pct)) + 
  geom_point(alpha = 0.1)

ggplot(data=data, aes(x=exceed100perc, y=cfu2)) + 
  geom_point(alpha = 0.2)

ggplot(data=data, aes(x=cfu2, y=exceed100perc)) + 
  geom_point(alpha = 0.2)

ggplot(data=data, aes(x=cfu2, y=cfuGeomMean)) + 
  geom_point(alpha = 0.2)

par(mfrow = c(1, 2))
boxplot(data$cfu2)
boxplot(data$exceed100perc)

ggplot(data=data, aes(x=total, y=white_pct)) + 
  geom_point(alpha = 0.1)

ggplot(data=data, aes(x=black_pct, y=white_pct)) + 
  geom_point(alpha = 0.1)

ggplot(data=data, aes(x=exceed100perc, y=n)) + 
  geom_point()

ggplot(data=data, aes(x=exceed100perc, y=n)) + 
  geom_point(alpha = 0.5)

ggplot(data=data, aes(x=cfu2, y=n)) + 
  geom_point(alpha = 0.1)

ggplot(data=data, aes(x=exceed100perc, y=total)) + 
  geom_point(alpha = 0.1)


ggplot(data = data, aes(x = exceed100perc, y = n)) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "exceed100perc", y = "n") +
  theme_bw()

# slice up data 

# look for nulls 
sapply(data, function(x) sum(is.na(x)))
#no nulls

# look at correlations 
round(cor(data[c('exceed100perc', 'cfu2')]), 2)

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


'
  _ _                                             _      _     
 | (_)_ __   ___  __ _ _ __   _ __ ___   ___   __| | ___| |___ 
 | | | `_ \ / _ \/ _` | `__| | `_ ` _ \ / _ \ / _` |/ _ \ / __|
 | | | | | |  __/ (_| | |    | | | | | | (_) | (_| |  __/ \__ \
 |_|_|_| |_|\___|\__,_|_|    |_| |_| |_|\___/ \__,_|\___|_|___/
                                                   

'

# weighted linear regression
# weights by total visitation 
# commonly used to reduce heteroskedacity or downweight imprecise/ low quality data points
# sensitive to outliers


reg_l = lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income,data=data)
reg_lw = lm(exceed100perc_x100~white_pct + hispanic_or_latino_pct + med_household_income_k,weights = total,data=data)
summary(reg_lw)
vif(reg_lw)
# sig white and hispanic/latino 
# poor wq and pct hispanic/latino negatively correlated, interestingly 
# vifs fine 
# has weighting improved heteroskedacity issue? 
plot(reg_lw$fitted.values, reg5w$residuals, xlab = "Fitted values", ylab = "Residuals")
# these are also weird looking
hist(reg_lw$residuals, breaks = 30) 
#weird residuals
# more diagnostics 
par(mfrow = c(2, 2))
plot(reg_lw)
par(mfrow = c(1, 1))
extractAIC(reg_lw)
extractAIC(lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data, weights = total))
extractAIC(lm(exceed100perc~white_pct + hispanic_or_latino_pct,data=data, weights = total))



#log transforming exceedences
reg_lwlog = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total)
summary(reg_lwlog)
hist(reg_lwlog$residuals, breaks = 30)
par(mfrow = c(2, 2))
plot(reg_lwlog)



# cfu 
regc = lm(log(cfu2)~white_pct + hispanic_or_latino_pct,data=data, weights = total)
summary(regc)
regcNoLog = lm(cfu2~white_pct + hispanic_or_latino_pct,data=data, weights = total)
summary(regcNoLog)

regcg = lm(log(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total)
summary(regcg)

regcg_log10 = lm(log10(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total)
summary(regcg_log10)



par(mfrow = c(2, 2))
plot(regcg)




#stargazer output 
# add components to model objects 
reg_lw$AIC = AIC(reg5w)
reg_lwlog$AIC = AIC(reg5wlog)
regcg$AIC = AIC(reg5wlog)
stargazer(reg_lw,reg_lwlog, regcg, header=FALSE,title="My Nice Regression Table", 
          type='text',
          keep.stat = c("n", "rsq", "adj.rsq", "ll", "f", "ser", "aic"),
          #keep.stat = c("all"),
          digits=3)

# get interpretable marginal effects for log transformed models 
margins(regcg)
margins(reg_lwlog)


# To calculate the Marginal Effect at the Mean (MEM) we have to obtain the mean values for each variable

data_means_list = lapply(data,mean,na.rm=T)
margins(regcg,at = mean(data$white_pct))
margins(reg_lwlog,at = data_means_list)

# predictions 
#define new observation
newdata1 = data.frame(white_pct=87, hispanic_or_latino_pct=5, med_household_income_k = 50)
newdata2 = data.frame(white_pct=77, hispanic_or_latino_pct=5, med_household_income_k = 50)
#use model to predict points value
morewhite = exp(predict(reg_lwlog, newdata1))
lesswhite = exp(predict(reg_lwlog, newdata2))
morewhite - lesswhite



'
 _ _ _                               
(_) | |_ __   ___  ___ ___  ___  ___ 
| | | | `_ \ / _ \/ __/ __|/ _ \/ __|
| | | | | | |  __/\__ \__ \  __/\__ \
|_|_|_|_| |_|\___||___/___/\___||___/
  
'


#general shape of rate function 
# (from Dofour 1984)
ggplot(data.frame(x=c(0, 1000)), aes(x)) + 
  labs(x = "mean enterococci CFU", y = "illnesses per 1000 swimmers") + 
  stat_function(fun=function(x) 0.20 + (12.17*log10(x)))

# function to put together our fitted cfu regression and rate function
combineCfuRateEqs <- function(const, coeff1, coeff2, coeff3){
  print(c("R = ", 0.20 + (12.17*const), " + ", coeff1*12.17, "*x1 + ", coeff2*12.17, "*x2 + ", coeff3*12.17 ))
}
combineCfuRateEqs(2.68, -0.02, -0.0059, -0.0019)

# define function to quickly turn CFUs into rates 
makeRate <- function(cfu){
  0.20 + (12.17*log10(cfu))
}

rate_per1k_white = makeRate(weighted.mean(data$cfuGeomMean, data$white_pct))
rate_per1k_black = makeRate(weighted.mean(data$cfuGeomMean, data$black_pct))
rate_per1k_hispanic = makeRate(weighted.mean(data$cfuGeomMean, data$hispanic_or_latino_pct))






## weight experiments 

# make new total variable that is on a smaller scale (out of 1)

data$total_rescale = data$total / 14980573


# run regs with this total as weight rather than actual total 






#regular 
reg5wlog = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total)
summary(reg5wlog)
#rescaled 
reg5wlog_rescaledw = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total_rescale)
summary(reg5wlog_rescaledw)



#regular
reg5w = lm(exceed100perc_x100~white_pct + hispanic_or_latino_pct + med_household_income_k,weights = total,data=data)
summary(reg5w)
#rescaled
reg5w_rescaledw = lm(exceed100perc_x100~white_pct + hispanic_or_latino_pct + med_household_income_k,weights = total_rescale,data=data)
summary(reg5w_rescaledw)

