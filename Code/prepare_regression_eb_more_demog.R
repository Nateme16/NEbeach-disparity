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
census_api_key("aa5b0c4e8e217972b3b77d48c78ba980a2a5c0cd", install=TRUE,overwrite=TRUE)
readRenviron("~/.Renviron")

statelist=c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
#statelist=c('AL','AK')

trips$GEOID=trips$Home


allvarlist <- load_variables(2018, "acs5", cache = TRUE)

# a labeled array woud be easier?
# varlist=c("B02001_001",	"B02001_002")
# varname=c("Estimate!!Total","Estimate!!Total!!White alone")

# census codes with labels 
varlabels = list('B01003_001' = 'tot_pop',

  
                 'B25003_001' = 'tenure_tot_pop',
                 'B25003_002' = 'owner_occupied',
                 
             'B02001_001' = 'race_tot_pop',
             'B02001_002' = 'white_alone',
             'B02001_003' = 'black_afam_alone',
             'B02001_004' = 'am_ind_alsk_ntv_alone',
             'B02001_005' = 'asian_alone',
             'B02001_006' = 'ntv_hwi_pac_isl_alone',
             'B02001_007' = 'some_other_race_alone',
             'B02001_008' = 'two_or_more_races',
             'B02001_009' = 'two_races_including',
             'B02001_010' = 'two_races_excluding',
             
             'B19013_001' = 'med_household_income',
             
             'B03003_001' = 'hispanic_or_latino_tot_pop',
             'B03003_003' = 'hispanic_or_latino_n',
             
             'B03002_001' = 'hispanic_or_latino_race_tot_pop',
             'B03002_003' = 'hlr_white_alone',
             'B03002_004' = 'hlr_black_afam',
             'B03002_005' = 'hlr_amer_ind_alsk_ntv',
             'B03002_006' = 'hlr_asian',
             
             'B25077_001' = 'median_home_value'
             )
# make list with just the codes 
varlist = names(varlabels)

#iterate through states
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

# process census vars to get meaningful vars for regression
dem_m=dem_m[dem_m$B02001_001>0,]

dem_m$tot_pop = dem_m$B01003_001 # copy total popultation

dem_m$owner_occ_pct = dem_m$B25003_002 / dem_m$B25003_001 # create % owner occupied

dem_m$white_pct = dem_m$B02001_002 / dem_m$B02001_001 #create %white
dem_m$black_afam_pct = dem_m$B02001_003 / dem_m$B02001_001 #create %black
dem_m$am_ind_ak_ntv_pct = dem_m$B02001_004 / dem_m$B02001_001 #create %american indian/alaska native
dem_m$asian_pct = dem_m$B02001_005 / dem_m$B02001_001 #create %asian
dem_m$ntv_hw_pac_isl_pct = dem_m$B02001_006 / dem_m$B02001_001 #create %native hawaiian/pacific islander
dem_m$some_other_race_pct = dem_m$B02001_007 / dem_m$B02001_001 #create %some other race 
dem_m$two_or_more_races_pct = dem_m$B02001_008 / dem_m$B02001_001 #create %two or more races 
dem_m$two_races_including_pct = dem_m$B02001_009 / dem_m$B02001_001 #create %two races including...
dem_m$two_races_excluding_pct = dem_m$B02001_010 / dem_m$B02001_001 #create %two races excluding...

dem_m$med_household_income = dem_m$B19013_001 # copy median household income

dem_m$hispanic_or_latino_pct = dem_m$B03003_003 / dem_m$B03003_001 #create %hispanic or latino

dem_m$not_h_white_pct = dem_m$B03003_003 / dem_m$B03002_001 #create %non-hispanic or latino white
dem_m$not_h_black_pct = dem_m$B03003_004 / dem_m$B03002_001 #create %non-hispanic or latino black/african american
dem_m$not_h_ai_an_pct = dem_m$B03003_005 / dem_m$B03002_001 #create %non-hispanic or latino %american indian/alaska native
dem_m$not_h_asian_pct = dem_m$B03003_006 / dem_m$B03002_001 #create %non-hispanic or latino black/african american

dem_m$med_home_value = dem_m$B25077_001 #copy median home value

#would be cool to add a timestamp to the end of this 
save(dem_m,file="data/block_demographics.Rdata")
#load(file="data/block_demographics.Rdata")

dem=dem_m

#dem=dem[dem$trips>0,]

#"trip-weight"/apportion census vars
percent_vars = list(
                    dem$white_pct,
                    dem$black_afam_pct,
                    dem$am_ind_ak_ntv_pct,
                    dem$asian_pct,
                    dem$ntv_hw_pac_isl_pct,
                    dem$some_other_race_pct,
                    dem$two_or_more_races_pct,
                    dem$two_races_including_pct,
                    dem$two_races_excluding_pct,
                    
                    dem$hispanic_or_latino_pct,
                    
                    dem$not_h_white_pct,
                    dem$not_h_black_pct,
                    dem$not_h_ai_an_pct,
                    dem$not_h_asian_pct)
                    
for (vars in percent_vars){
  var_as_string = deparse(substitute(vars))
  var_as_string = substring(var_as_string, 6)
  dem[paste(var_as_string, "_trips")] = dem$trips*vars
}

# dem$owner_occ_trips = dem$trips*dem$owner_occ_pct
# dem$whitetrips=dem$trips*dem$white_pct
# dem$black_afam_trips=dem$trips*dem$black_afam_pct

#weight medians here -- multiply by trips. THis is not a meaningful number 
# but helps get weighted median income for beaches later
dem$med_household_income_xtrips = dem$med_household_income * dem$trips 

dem$med_home_value_xtrips = dem$med_home_value * dem$trips


#collapse demographics to beaches

dem2= dem %>%
  group_by(Poi,year) %>%
  # indicate how to collapse each census var to beach
 
  summarise(total=sum(trips),
            
            whitetripsbeach=sum(white_pct_trips),
            white_pct= whitetripsbeach/total,
            
            blacktripsbeach=sum(black_afam_pct_trips),
            black_pct= blacktripsbeach/total,
            
            am_ind_ak_ntv_tripsbeach=sum(am_ind_ak_ntv_pct_trips),
            am_ind_ak_ntv_pct= am_ind_ak_ntv_tripsbeach/total,
            
            asiantripsbeach=sum(asian_pct_trips),
            asian_pct= asiantripsbeach/total,
            
            ntv_hw_pac_isl_tripsbeach=sum(ntv_hw_pac_isl_pct_trips),
            ntv_hw_pac_isl_pct= ntv_hw_pac_isl_tripsbeach/total,
            
            some_other_race_tripsbeach=sum(some_other_race_pct_trips),
            some_other_race_pct= some_other_race_pct_tripsbeach/total,
            
            two_or_more_races_tripsbeach=sum(two_or_more_races_pct_trips),
            two_or_more_races_pct= two_or_more_races_tripsbeach/total,
            
            two_races_including_tripsbeach=sum(two_races_including_pct_trips),
            two_races_including_pct= two_races_including_tripsbeach/total,
            
            two_races_excluding_tripsbeach=sum(two_races_excluding_pct_trips),
            two_races_excluding_pct= two_races_excluding_tripsbeach/total,
            
            hispanic_or_latino_tripsbeach=sum(hispanic_or_latino_pct_trips),
            hispanic_or_latino_pct= hispanic_or_latino_tripsbeach/total,
            
            not_h_white_tripsbeach=sum(not_h_white_pct_trips),
            not_h_white_pct= not_h_white_tripsbeach/total,
            
            not_h_black_tripsbeach=sum(not_h_black_pct_trips),
            not_h_black_pct= not_h_black_tripsbeach/total,
            
            not_h_ai_an_tripsbeach=sum(not_h_ai_an_pct_trips),
            not_h_ai_an_pct= not_h_ai_an_tripsbeach/total,
            
            not_h_asian_tripsbeach=sum(not_h_asian_pct_trips),
            not_h_asian_pct= not_h_asian_tripsbeach/total,
            
            median_income_xtripsbeach=sum(med_household_income_xtrips),
            med_household_income= median_income_xtripsbeach/total,
            
            med_home_value_xtripsbeach=sum(med_home_value_xtrips),
            med_home_value= med_home_value_xtripsbeach/total,
            
            
  )

#connect wq to beaches
#Nate needs to fix 
wq=read.csv("data/bacteria_yearly.csv")
dem2$poi=dem2$Poi
data=merge(wq,dem2,by=c("poi"))

#Look at correlations bw variables, make scatterplots

#regress wq on demographics
reg1=lm(exceed100perc~white,data=data)
summary(reg1)

