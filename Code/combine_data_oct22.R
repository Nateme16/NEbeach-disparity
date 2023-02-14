#Creating Beach Visitation for New England with first and missing days from Airsage#
library(here)
library(gtools)
library(dplyr)
library(R.utils)
library(readr)

rm(list=ls()) #clear all

####Create POI summary info for all months together####

#build path to data files sensitive to usernames in OneDrive link
wd= paste("C:/Users/",Sys.getenv("USERNAME"),"/Environmental Protection Agency (EPA)/ACESD Social Science Team - Documents/General/Research Projects/Beach research STRAP4/New England beach cell data/NEbeach-disparity/data", sep = "")

setwd(wd)

file_list <- list.files(pattern="poi_summary*", recursive = TRUE)

for (i in 1:length(file_list)){
  nameList <- (unlist(strsplit(file_list[i], "_")))
  filename <- paste(nameList[3], nameList[4], sep="_") #file name for poi_summary files
  #filename <- paste(nameList[2], nameList[3], sep="_") #file name for all other report files
  # Make a copy of the first dataset as the base dataset for merging
  if (i == 1){
    dataset <- read.table(file_list[i], header=TRUE, sep=',')}
  
  # Append all other datasets to the base dataset
  if (i !=  1){
    temp_dataset <-read.table(file_list[i], header=TRUE, sep=',')
    dataset <-smartbind(dataset, temp_dataset)
    rm(temp_dataset) #remove temporary dataset
  }
  print(filename)}

dataset$YEAR <- as.character(substr(dataset$DATE, 1, 4))
dataset$MONTH <- as.character(substr(dataset$DATE, 5, 6))
dataset$DAY <- as.character(substr(dataset$DATE, 7, 8))

write.csv(dataset,"Data/poi_combined_NewEngland_oct22.csv")
save(dataset,file="poi_combined_NewEngland_oct22.Rdata")


####Create HOME information for each POI for all months together?####

file_list <- list.files(pattern="home*", recursive = TRUE)

for (i in 1:length(file_list)){
  nameList <- (unlist(strsplit(file_list[i], c("-"))))
  nameList <- (unlist(strsplit(nameList, c("/"))))
  
  filename <- paste(nameList[4]) #file name month for HOME files

  # Make a copy of the first dataset as the base dataset for merging
  
  if (i == 1){
    dataset <- read.table(file_list[i], header=TRUE, sep=',')[,c(1,2,32:50)]
    dataset$month=filename}
  
  # Append all other datasets to the base dataset
  if (i !=  1){
    temp_dataset <-read.table(file_list[i], header=TRUE, sep=',')[,c(1,2,32:50)]
    temp_dataset$month=filename
    dataset <-smartbind(dataset, temp_dataset)
    rm(temp_dataset) #remove temporary dataset
  }
  
  print(filename) }

dataset$YEAR <- as.character(substr(dataset$month, 1, 4))
dataset$MONTH <- as.character(substr(dataset$month, 5, 6))

test=dataset[1:100,]

write.csv(dataset,"Data/home_combined_NewEngland_oct22.csv")
save(dataset,file="home_combined_NewEngland_oct22.Rdata")

####Frequency by duration issues to get total visits by census block by month instead of unique people per month (people can go multiple times, but would only be counted once in the HOME file)####

file_list <- list.files(pattern="byHome*", recursive = TRUE)

for (i in 1:length(file_list)){
  nameList <- (unlist(strsplit(file_list[i], c("-"))))
  nameList <- (unlist(strsplit(nameList, c("/"))))
  
  filename <- paste(nameList[4]) #file name month for HOME files
  
  # Make a copy of the first dataset as the base dataset for merging
  
  if (i == 1){
    dataset <- read_csv(file_list[i])
    
    dataset2=dataset[,c(1,2,4:32)]
    
    dataset2= dataset2 %>%
      group_by(Poi,Home) %>%
      summarise(across(everything(), sum))
    
    dataset2$month=filename
    
    totaltrips= data.frame(c(dataset2[,c(5:11)], 3*dataset2[,c(12:18)], 6*dataset2[,c(19:25)] , 8*dataset2[,c(26:31)]))
    totaltrips$total=rowSums(totaltrips)
    
    dataset2$total=totaltrips$total
    
    trips_by_blockgroup=dataset2[,c("month","Poi","Home","total")]
    rm(totaltrips)
    }
  
  # Append all other datasets to the base dataset
  if (i !=  1){
    dataset <- read_csv(file_list[i]) 
    
    dataset2=dataset[,c(1,2,4:32)]
    
    dataset2= dataset2 %>%
      group_by(Poi,Home) %>%
      summarise(across(everything(), sum))
    
    dataset2$month=filename
    
    totaltrips= data.frame(c(dataset2[,c(5:11)], 3*dataset2[,c(12:18)], 6*dataset2[,c(19:25)] , 8*dataset2[,c(26:31)]))
    totaltrips$total=rowSums(totaltrips)
    
    dataset2$total=totaltrips$total
    
    trips_by_blockgroup_temp=dataset2[,c("month","Poi","Home","total")]
    
    trips_by_blockgroup <-rbind(trips_by_blockgroup,trips_by_blockgroup_temp)
    rm(trips_by_blockgroup_temp,totaltrips) #remove temporary dataset
  }
  
  print(filename) }

trips_by_blockgroup$year=as.character(substr(trips_by_blockgroup$month, 1, 4))

write.csv(trips_by_blockgroup,"Data/trips_by_blockgroup_NewEngland_oct22.csv")
save(trips_by_blockgroup,file="trips_by_blockgroup_NewEngland_oct22.Rdata")

#collapse months to total for each year/season?
trips_by_blockgroup_yearly= trips_by_blockgroup %>%
  group_by(Poi,Home,year) %>%
  summarise(trips= sum(total))

#both years
write.csv(trips_by_blockgroup_yearly,"Data/trips_by_blockgroup_yearly_NewEngland_oct22.csv")
save(trips_by_blockgroup_yearly,file="Data/trips_by_blockgroup_yearly_NewEngland_oct22.Rdata")

#2018
write.csv(trips_by_blockgroup_yearly[trips_by_blockgroup_yearly$year=="2018",],"Data/trips_by_blockgroup_yearly_2018_NewEngland_oct22.csv")
save(trips_by_blockgroup_yearly,file="Data/trips_by_blockgroup_yearly_2018NewEngland_oct22.Rdata")

#2019
write.csv(trips_by_blockgroup_yearly[trips_by_blockgroup_yearly$year=="2019",],"Data/trips_by_blockgroup_yearly_2019_NewEngland_oct22.csv")
save(trips_by_blockgroup_yearly,file="Data/trips_by_blockgroup_yearly_2019_NewEngland_oct22.Rdata")



