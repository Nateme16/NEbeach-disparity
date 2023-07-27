#prepare a regression for wq=f(demographics)

'
  ____                                 _   _             
 |  _ \ _ __ ___ _ __   __ _ _ __ __ _| |_(_) ___  _ __  
 | |_) | `__/ _ \ ,_ \ / _` | `__/ _` | __| |/ _ \| `_ \ 
 |  __/| | |  __/ |_) | (_| | | | (_| | |_| | (_) | | | |
 |_|   |_|  \___| .__/ \__,_|_|  \__,_|\__|_|\___/|_| |_|
                |_|                                      
Run all of this first 
'
# load packages
pacman::p_load(here, pscl, boot, dplyr, tidyverse, car, lmtest, stargazer, AER, 
               MASS, broom, margins, dotwhisker)

rm(list=ls()) #clear environment 

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

# make median household income in thousands so coefficients 
# are easier to interpret 
data$med_household_income_k = data$med_household_income / 1000
#make pct exceedances out of 100 
data$exceed100perc_x100 = data$exceed100perc * 100

# make subsets removing outliers 
# excluding 3 largest beaches 
data_minus3 = subset(data, total < 9500000)
# excluding 11 visitations above 5 million
data_minus11 = subset(data, total < 5000000)
# excluding top 10% largest beaches 
data_minus10pct = subset(data, total < 1556797.54)


'
  _____ ____    _     
 | ____|  _ \  / \    
 |  _| | | | |/ _ \   
 | |___| |_| / ___ \  
 |_____|____/_/   \_\ 
                      
  Exploratory Data Analysis                      

'

#exploratory data analysis 
head(data)
summary(data)

# histogram and quantiles of visitation, showing potential outlier cutoffs 
ggplot(data=data, aes(x=total)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of total visitation")

quantile(data$total, probs = seq(0, 1, 0.05), na.rm = FALSE)


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



# Models of logged exceedances 
    # Unweighted, entire dataset
    r_exceed_noweights = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data)
    summary(r_exceed_noweights)
    # Weighted, entire dataset
    r_exceed_alldata =lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total)
    summary(r_exceed_alldata)
    # Weighted, "minus 3" subset
    r_exceed_minus3 = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data_minus3, weights = total)
    summary(r_exceed_minus3)
    # Weighted, "minus 11" subset
    r_exceed_minus11 = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data_minus11, weights = total)
    summary(r_exceed_minus11)
    # Weighted, "minus 10 percent" subset 
    r_exceed_minus10pct = lm(log(exceed100perc_x100+1)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data_minus10pct, weights = total)
    summary(r_exceed_minus10pct)
# Models of logged CFU
    # Unweighted, entire dataset
    r_cfu_noweights = lm(log(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data)
    summary(r_cfu_noweights)
    # Weighted, entire dataset
    r_cfu_alldata = lm(log(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data, weights = total)
    summary(r_cfu_alldata)
    # Weighted, "minus 3" subset
    r_cfu_minus3 = lm(log(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data_minus3, weights = total)
    summary(r_cfu_minus3)
    # Weighted, "minus 11" subset
    r_cfu_minus11 = lm(log(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data_minus11, weights = total)
    summary(r_cfu_minus11)
    # Weighted, "minus 10 percent" subset 
    r_cfu_minus10pct = lm(log(cfuGeomMean)~white_pct + hispanic_or_latino_pct + med_household_income_k,data=data_minus10pct, weights = total)
    summary(r_cfu_minus10pct)
    
    
# Model diagnostics and plots 

# general diagnostics for choosing among functional forms, etc 

par(mfrow = c(2, 2)) # makes the figure window a 2x2 grid so you can see all 4 plots
plot(r_cfu_alldata)
plot(r_cfu_noweights)
plot(r_cfu_minus11)


# regression coefficients plot
# for CFU
dwplot(list(r_cfu_noweights, r_cfu_alldata, r_cfu_minus11),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )
       ) %>% # plot line at zero _behind_coefs
         relabel_predictors(
           c(
             white_pct = "Percent white",
             hispanic_or_latino_pct = "Percent Hispanic or Latino",
             med_household_income_k = "Median household income (thousands)"
           )
         
       )



# for exceedances 
dwplot(list(r_exceed_noweights, r_exceed_alldata, r_exceed_minus11),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )
) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      white_pct = "Percent white",
      hispanic_or_latino_pct = "Percent Hispanic or Latino",
      med_household_income_k = "Median household income (thousands)"
    )
    
  )








# predictions -- useful for interpreting log linear regression coefficients
# for now, predicting for weighted regressions based on the "minus 11" subset, 
# and for a scenario when var of interest increases/ decreases from median by 10 percent / $10k
# but this section is flexibleish

#define new example observation -- vary variable of interest and hold others constant 
whiterObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 96)
lesswhiteObs = data.frame(white_pct=78, hispanic_or_latino_pct=5, med_household_income_k = 96)

lessHispanicObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 96)
moreHispanicObs = data.frame(white_pct=88, hispanic_or_latino_pct=15, med_household_income_k = 96)

richerObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 96)
poorerObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 86)

#use model to predict change in exceedance/cfu in scenario
'example interpretion: A 10% increase/decrease (from 12% to 22%) in the chance that a 
beach visit is by a non-white person is associated with a x% increase/decrease in the 
number of exceedances at that beach
'
lessWhitePred_Exceedance = exp(predict(r_exceed_minus11, lesswhiteObs )) - exp(predict(r_exceed_minus11, whiterObs))
MoreHispanicPred_Exceedance = exp(predict(r_exceed_minus11, moreHispanicObs )) - exp(predict(r_exceed_minus11, lessHispanicObs))
lessRichPred_Exceedance =   exp(predict(r_exceed_minus11, poorerObs )) - exp(predict(r_exceed_minus11, richerObs))
  
lessWhitePred_CFU = exp(predict(r_cfu_minus11, lesswhiteObs )) - exp(predict(r_cfu_minus11, whiterObs))
MoreHispanicPred_CFU = exp(predict(r_cfu_minus11, moreHispanicObs )) - exp(predict(r_cfu_minus11, lessHispanicObs))
lessRichPred_CFU =  exp(predict(r_cfu_minus11, poorerObs )) - exp(predict(r_cfu_minus11, richerObs))







#stargazer output 


# add AICs to model objects 
# could be a for loop but I don't understand for loops in r 
  
r_cfu_alldata$AIC = AIC(r_cfu_alldata)
r_cfu_noweights$AIC = AIC(r_cfu_noweights)
r_cfu_minus10pct$AIC = AIC(r_cfu_minus10pct)
r_cfu_minus11$AIC = AIC(r_cfu_minus11)
r_cfu_minus3$AIC = AIC(r_cfu_minus3)

r_exceed_alldata$AIC = AIC(r_exceed_alldata)
r_exceed_noweights$AIC = AIC(r_exceed_noweights)
r_exceed_minus10pct$AIC = AIC(r_exceed_minus10pct)
r_exceed_minus11$AIC = AIC(r_exceed_minus11)
r_exceed_minus3$AIC = AIC(r_exceed_minus3)


# Table for exceedence models
stargazer(r_exceed_noweights, r_exceed_alldata, r_exceed_minus11, header=FALSE,title="Log of percent exceedances regressions", 
          type='text',
          column.labels=c("Unweighted", "Weighted, all obs.","Weighted, outliers removed"),
          covariate.labels = c("Percent white", "Percent Hispanic or Latino","Median household income (thousands)"),
          dep.var.labels = c("" ),
          keep.stat = c("n", "rsq", "adj.rsq", "ll", "f", "ser", "aic"),
          digits=3)

# Table for CFU models 

stargazer(r_cfu_noweights, r_cfu_alldata, r_cfu_minus11, header=FALSE,title="Log of mean CFU regressions", 
          type='text',
          column.labels=c("Unweighted", "Weighted, all obs.","Weighted, outliers removed"),
          covariate.labels = c("Percent white", "Percent Hispanic or Latino","Median household income (thousands)"),
          dep.var.labels = c("" ),
          keep.stat = c("n", "rsq", "adj.rsq", "ll", "f", "ser", "aic"),
          digits=3)



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


# define function to quickly turn CFUs into rates 
makeRate <- function(cfu){
  0.20 + (12.17*log10(cfu))
}

# rates for all data 
rate_per1k_white_alldata = makeRate(weighted.mean(data$cfuGeomMean, data$white_pct))
rate_per1k_black_alldata = makeRate(weighted.mean(data$cfuGeomMean, data$black_pct))
rate_per1k_hispanic_alldata = makeRate(weighted.mean(data$cfuGeomMean, data$hispanic_or_latino_pct))

# rates for "minus 3" subset 
rate_per1k_white_minus3 = makeRate(weighted.mean(data_minus3$cfuGeomMean, data_minus3$white_pct))
rate_per1k_black_minus3 = makeRate(weighted.mean(data_minus3$cfuGeomMean, data_minus3$black_pct))
rate_per1k_hispanic_minus3 = makeRate(weighted.mean(data_minus3$cfuGeomMean, data_minus3$hispanic_or_latino_pct))


# rates for "minus 11" subset 
rate_per1k_white_minus11 = makeRate(weighted.mean(data_minus11$cfuGeomMean, data_minus11$white_pct))
rate_per1k_black_minus11 = makeRate(weighted.mean(data_minus11$cfuGeomMean, data_minus11$black_pct))
rate_per1k_hispanic_minus11 = makeRate(weighted.mean(data_minus11$cfuGeomMean, data_minus11$hispanic_or_latino_pct))

# rates for "minus 10 percent" subset 
rate_per1k_white_minus10pct = makeRate(weighted.mean(data_minus10pct$cfuGeomMean, data_minus10pct$white_pct))
rate_per1k_black_minus10pct = makeRate(weighted.mean(data_minus10pct$cfuGeomMean, data_minus10pct$black_pct))
rate_per1k_hispanic_minus10pct = makeRate(weighted.mean(data_minus10pct$cfuGeomMean, data_minus10pct$hispanic_or_latino_pct))







