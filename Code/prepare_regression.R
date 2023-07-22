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
               MASS, broom, margins)

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
# excluding 11 visitations above 5 million, a possible outlier cutoff
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

# histogram of visitation, showing potential outlier cutoffs 
ggplot(data=data, bins = 100, aes(x=total)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of total visitation") 


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







# predictions -- useful for interpreting log linear regression coefficients
# for now, predicting for weighted regressions based on the "minus 11" subset, 
# and for a scenario when var of interest increases/ decreases from median by 10 percent / $10k
# but this section is flexible

#define new example observation -- vary variable of interest and hold others constant 
whiterObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 96)
lesswhiteObs = data.frame(white_pct=78, hispanic_or_latino_pct=5, med_household_income_k = 96)

lessHispanicObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 96)
moreHispanicObs = data.frame(white_pct=88, hispanic_or_latino_pct=15, med_household_income_k = 96)

richerObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 96)
poorerObs = data.frame(white_pct=88, hispanic_or_latino_pct=5, med_household_income_k = 86)

#use model to predict change in exceedance/cfu in scenario
'example interpretion: A 10% increase (from 12% to 22%) in the chance that a 
beach visit is by a non-white person is associated with a 3.7% increase in the 
number of exceedances at that beach
'
lessWhitePred_Exceedance = exp(predict(r_exceed_minus11, whiterObs )) - exp(predict(r_exceed_minus11, lesswhiteObs))
MoreHispanicPred_Exceedance = exp(predict(r_exceed_minus11, lessHispanicObs )) - exp(predict(r_exceed_minus11, moreHispanicObs))
lessRichPred_Exceedance =   exp(predict(r_exceed_minus11, richerObs )) - exp(predict(r_exceed_minus11, poorerObs))
  
lessWhitePred_Exceedance = exp(predict(r_cfu_minus11, whiterObs )) - exp(predict(r_cfu_minus11, lesswhiteObs))
MoreHispanicPred_Exceedance = exp(predict(r_cfu_minus11, lessHispanicObs )) - exp(predict(r_cfu_minus11, moreHispanicObs))
lessRichPred_Exceedance =  exp(predict(r_cfu_minus11, richerObs )) - exp(predict(r_cfu_minus11, poorerObs))


#stargazer output 
# add components to model objects 
reg_lw$AIC = AIC(reg5w)
reg_lwlog$AIC = AIC(reg5wlog)
regcg$AIC = AIC(reg5wlog)

# Table for exceedence models 
stargazer(reg_lw,reg_lwlog, regcg, header=FALSE,title="Regression table", 
          type='text',
          keep.stat = c("n", "rsq", "adj.rsq", "ll", "f", "ser", "aic"),
          #keep.stat = c("all"),
          digits=3)

# Table for CFU models 


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

rate_per1k_white = makeRate(weighted.mean(data$cfuGeomMean, data$white_pct))
rate_per1k_black = makeRate(weighted.mean(data$cfuGeomMean, data$black_pct))
rate_per1k_hispanic = makeRate(weighted.mean(data$cfuGeomMean, data$hispanic_or_latino_pct))










