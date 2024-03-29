#Create WQ variable from beacon data

library(here)
library(dplyr)

rm(list=ls()) #clear all

####Create POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

setwd(wd)

bacteria=read.csv("beachAirsageJoined_03-09-2023-214244PM.csv")

bacteria=bacteria[,c("ActivityStartDate","ResultMeasureValue","Unique_ID" )]
bacteria=bacteria[bacteria$ActivityStartDate!="",] #ask Erin about the blank activity start date

bacteria$ResultMeasureValue[is.na(bacteria$ResultMeasureValue)]=0
bacteria$cfu=as.numeric(bacteria$ResultMeasureValue)
bacteria$cfuNoZeros = bacteria$cfu
#replace zeros with ones so geometric mean is possible 
bacteria$cfuNoZeros[bacteria$cfu == 0] <- 1

bacteria=bacteria[!is.na(bacteria$cfu),]

bacteria$poi=bacteria$Unique_ID

bacteria$year <- as.character(substr(bacteria$ActivityStartDate, 1, 4))
bacteria$month <- as.character(substr(bacteria$ActivityStartDate, 6, 7))
bacteria$day <- as.character(substr(bacteria$ActivityStartDate, 9, 10))

bacteria=bacteria[,c("poi","cfu", "cfuNoZeros", "year","month","day")]

#collapse to year- 
bacteria2= bacteria %>%
  group_by(poi,year) %>%
  summarise(cfu2=mean(cfu),
            cfuGeomMean = exp(mean(log(cfuNoZeros))),
            n=n(),
            exceed100=sum(cfu > 100),
            exceed100perc=exceed100/n)

bacteria2=bacteria2[,c("poi","cfu2", "cfuGeomMean", "exceed100","exceed100perc","n","year")]

write.csv(bacteria2,"bacteria_yearly.csv")


#collapse to window of years- 
bacteria_window= bacteria %>%
  group_by(poi) %>%
  summarise(cfu2=mean(cfu),
            cfuGeomMean = exp(mean(log(cfuNoZeros))),
            n=n(),
            exceed100=sum(cfu > 100),
            exceed100perc=exceed100/n)

bacteria_window=bacteria_window[,c("poi","cfu2","cfuGeomMean", "exceed100","exceed100perc","n")]

write.csv(bacteria_window,"bacteria_window.csv")
ggplot(data=bacteria_window, aes(x=cfuGeomMean)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of cfus")













