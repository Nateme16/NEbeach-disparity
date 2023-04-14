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

# remove unsupported null types from df
data[is.na(data) | data=="Inf"] = NA

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

ggplot(data=data, aes(x=exceed100perc)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of exceedance percents")

#logged exceedences looks more normal 
ggplot(data=data, aes(x=log(exceed100perc))) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of logged exceedance percents")

ggplot(data=data, aes(x=cfu2)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of cfus")

#logged cfu also looks better
ggplot(data=data, aes(x=log(cfu2))) +
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
#none, though I feel like there should be some?

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


#regress wq on demographics
# first some simple, unweighted linear regressions

summary(lm(exceed100perc~white_pct,data=data))
# significant race 

summary(lm(exceed100perc~white_pct + med_household_income,data=data))
# significant race

summary(lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income,data=data))
# significant hispanic latino

reg4 = lm(exceed100perc~white_pct + black_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data)
summary(reg4)
# significant home value

# pausing to check some lm assumptions
vif(reg4)
#but VIFs are a bit high?
#check for heteroskedacity
plot(reg4$fitted.values, reg4$residuals, xlab = "Fitted values", ylab = "Residuals")
# it's definitely weird 

summary(lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data))
# significant home value?

# what about cfu as outcome?
summary(lm(cfu2~white_pct,data=data))
# N.S.

summary(lm(cfu2~white_pct + black_pct + hispanic_or_latino_pct + med_household_income,data=data))
# significant income 


# how about if we log cfu 
cfuregNoWeights = (lm(log(cfu2)~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data))
# significant home value and hispanic/latino, this time positive

#log cfu and add weights
cfureg = (lm(log(cfu2)~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data, weights = total))
summary(cfureg)
# this is a good model 

plot(cfureg$fitted.values, cfureg$residuals, xlab = "Fitted values", ylab = "Residuals")
# looks homoskedastic 

hist(cfureg$residuals, breaks = 30) 
# nice resids

#plot modeled vs actual 
data_modcfu <- data.frame(Predicted = exp(predict(cfureg)),  # Create data for ggplot2
                          Observed = data$cfu2)
ggplot(data_modcfu,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 0.5)
#outliers appear to be heavily impacting this 
#and weights too maybe 

# more diagnostics 

par(mfrow = c(2, 2))
plot(cfureg)
# QQ plot of residuals is sus 
# and some points have extremely high leverage


plot(cfuregNoWeights)
# but unweighted QQ plot is better, so it could be the weights 





# weighted linear regression
# weights by total visitation 
# commonly used to reduce heteroskedacity or downweight imprecise/ low quality data points
# sensitive to outliers

summary(lm(exceed100perc ~ white_pct, data = data, weights = total))
#significant race

summary(lm(exceed100perc ~ med_household_income, data = data, weights = total))
# N.S.

reg3w = lm(exceed100perc~white_pct + black_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data, weights = total)
summary(reg3w)
vif(reg3w)
# significant white and hispanic/latino
# concerning vifs 

reg4w = (lm(exceed100perc~white_pct + black_pct + asian_pct + two_or_more_races_pct + ntv_hw_pac_isl_pct + hispanic_or_latino_pct + am_ind_ak_ntv_pct + med_household_income + med_home_value,data=data, weights = total))
summary(reg4w)
vif(reg4w)
# wayyy too high VIFs (white_pct is 50), probably not a valid model 

reg5w = lm(exceed100perc~white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,data=data, weights = total)
summary(reg5w)
vif(reg5w)
# sig white and hispanic/latino 
# poor wq and pct hispanic/latino negatively correlated, interestingly 
# vifs fine 
# has weighting improved heteroskedacity issue? 
plot(reg5w$fitted.values, reg5w$residuals, xlab = "Fitted values", ylab = "Residuals")
# these are also weird looking

hist(reg5w$residuals, breaks = 30) 
#weird residuals

# more diagnostics 

par(mfrow = c(2, 2))
plot(reg5w)

# random forest? 
# Convert linear model to random forest model
# no weights (though weights are possible)
reg5rf <- randomForest(exceed100perc ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value, data = data)
print(reg5rf)
varImpPlot(reg5rf)
partialPlot(reg5rf, x.var = "hispanic_or_latino_pct", pred.data = 'exceed100perc')
# ^ fix doesnt work 


# can't log-linear-ify exceedances (as-is) because there are zeroes
# 



# generalized alternatives to linear model 
# may be better than log-transforming esp if have to deal w 0s
# beta regression (for proportion/percent outcomes) doesn't allow 0s....

#poisson-type as alternate to linear model? 
# pct exceedences can be treated like a count, with the denominator as an offset 
# this allows us to incorporate more information than just relying on a percent 
reg1p <- glm(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value, data = data, family = poisson)
dispersiontest(reg1p,trafo=1)
# seems overdispersed suggesting negbin is better, but let's try adding an offset 

reg1po <- glm(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value + offset(log(n)), data = data, family = poisson)
dispersiontest(reg1po,trafo=1)
#still overdispersed

# negative binomial (a generalization of poisson?) 

reg1bo <- glm.nb(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value + offset(log(n)), 
             data = data)
summary(reg1bo)
# home value only is sig

# but let's add weights back in 
reg1bow <- glm.nb(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value + offset(log(n)), 
                 data = data,
                 weights = total )
summary(reg1bow)
# everything is super significant but "Warning while fitting theta: alternation limit reached" 
# maybe didn't converge?
# neg bin may be challenging to use 
# zero-inflation?

# increase number of iterations to help convergence
reg2bow <- glm.nb(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value + offset(log(n)), 
                  data = data,
                  weights = total,
                  control=glm.control(maxit=50))
summary(reg2bow)
# no errors this time, but coefficient z and p values are suspicious
# to do: compare to poisson using LRT
# exponentiate coeffs
# "use the m1$resid command to obtain the residuals from our model to check other assumptions of the negative binomial model"


# test otherwise identical Poisson model to see which is better
regTestP <- glm(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value + offset(log(n)), 
                   data = data,
                   family = "poisson",
                   weights = total,
                   control=glm.control(maxit=50))
summary(regTestP)
pchisq(2 * (logLik(reg2bow) - logLik(regTestP)), df = 1, lower.tail = FALSE)
#what does this mean lol 

hist(reg2bow$residuals, breaks = 30) 
# not great, skewed 

#plot modeled vs actual 
data_modbow <- data.frame(Predicted = predict(reg2bow),  # Create data for ggplot2
                       Observed = data$exceed100,
                       Trips = data$total)
ggplot(data_modbow,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 0.5)
#bad?
# weights make this hard to interpret though

# color points by total

ggplot(data_modbow, aes(x = Predicted, y = Observed, color = Trips)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  scale_color_continuous(low = "yellow", high = "red")

# more "important" beaches are modeled more poorly....?

# trying zero-inflated negative binomial 

regzbow <- zeroinfl(exceed100 ~ white_pct + hispanic_or_latino_pct + med_household_income + med_home_value,
               data = data, dist = "negbin", 
               weights = total, offset = log(n)
               )
summary(regzbow)
# model seems to be mostly nans