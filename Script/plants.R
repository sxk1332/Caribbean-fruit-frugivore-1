setwd("../Data/PLANT")

library(BIEN)
library(maps) #Useful for making quick maps of occurrences
library(sp) # A package for spatial data
library(rgdal)
library(dplyr)
library(sf)
library(tidyverse)
library("spocc")
library("rgdal")
library("GISTools")

###### Extracting from BIEN #########
#https://github.com/bmaitner/RBIEN/blob/master/tutorials/RBIEN_tutorial.Rmd

### PLANTS

full_dat <- read.csv("final_plant_dat.csv", na.strings=c("", "NA"))
dat <- full_dat[which(full_dat$Origin == "native"),]

abiotic_dat <- dat[which(dat$Sindrome == "abiotic"),]
zoo_dat <- dat[which(dat$Sindrome == "zoo"),]

abiotic_list <- unique(abiotic_dat$Species)
zoo_list <- unique(zoo_dat$Species)

##Getting BIEN data for abiotic
setwd("./BIEN/abiotic")
BIEN_ranges_species(abiotic_list)   ## Takes a long time

# Getting list of plants to compare with reviewed fruit data  ## Takes a long time
temp <- list.files(getwd(), pattern= ".shp")
temp2 <- str_remove(temp, ".shp")
list_avail_plant <- str_replace(temp2, "_", " ")

#confirming that BIEN did not get data from outside the list
list_confirmed_plant <- subset(list_avail_plant, list_avail_plant %in% abiotic_list)

# Getting list of plants that we did not find from BIEN
list_absent_plant_abiotic <- subset(abiotic_list, !(abiotic_list %in% list_avail_plant))

################################################
##Getting BIEN data for zoo
setwd("../zoo")
BIEN_ranges_species(zoo_list)

# Getting list of plants to compare with reviewed fruit data
temp <- list.files(getwd(), pattern= ".shp")
temp2 <- str_remove(temp, ".shp")
list_avail_plant <- str_replace(temp2, "_", " ")

#confirming that BIEN did not get data from outside the list
list_confirmed_plant <- subset(list_avail_plant, list_avail_plant %in% zoo_list)

# Getting list of plants that we did not find from BIEN
list_absent_plant_zoo <- subset(zoo_list, !(zoo_list %in% list_avail_plant))

################ ABIOTIC POINTS
setwd("..")
setwd("../point")

for(i in c(1:length(list_absent_plant_abiotic))){
  results <- occ(query = as.character(list_absent_plant_abiotic[i]), from= c('gbif', 'bison', 'inat'), limit = 1000)
  res <- occ2df(results)
  
  print(paste0(list_absent_plant_abiotic[i]))
  print(i)
  
  if(length(res$name > 0)){ 
    tryCatch({
      res$longitude <- as.numeric(res$longitude)
      res$latitude <- as.numeric(res$latitude)
      
      res$time <- as.POSIXct(strptime(res$date, "%Y-%m-%d"))
      res$year <- format(strptime(res$date, "%Y-%m-%d"), '%Y')
      res$year <- as.numeric(res$year)
      
      #subsetting to the Caribbean area
      temp <- res[which(res$longitude > -92.8929 & res$longitude < -59.2015 & res$latitude < 27.303 & res$latitude > 8.2679),]
      
      temp2 <- temp[which(temp$year > 1987),] #removing all points older than 1988 (outside the range of data for KGC)
      temp3 <- temp2 %>% distinct(longitude, latitude, date, .keep_all = TRUE) #removing duplicates
      
      if(length(temp3$name > 0)){
        write.csv(temp3, paste0("plant_point_",list_absent_plant_abiotic[i], ".csv"))
        
        point <- st_as_sf(x = temp3, 
                          coords = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        st_write(point, paste0("point_",list_absent_plant_abiotic[i], ".shp"))
      } else{
        print(paste0(list_absent_plant_abiotic[i], " not enough points"))
      }
    }, error=function(e){})} else{
      print(paste0(list_absent_plant_abiotic[i], " not enough points"))
    }}

setwd("./abiotic")

################ ZOO POINTS
setwd("../zoo")

for(i in c(1:length(list_absent_plant_zoo))){
  results <- occ(query = as.character(list_absent_plant_zoo[i]), from= c('gbif', 'bison', 'inat'), limit = 1000)
  res <- occ2df(results)
  
  print(paste0(list_absent_plant_zoo[i]))
  print(i)
  
  if(length(res$name > 0)){ 
    tryCatch({
      res$longitude <- as.numeric(res$longitude)
      res$latitude <- as.numeric(res$latitude)
      
      res$time <- as.POSIXct(strptime(res$date, "%Y-%m-%d"))
      res$year <- format(strptime(res$date, "%Y-%m-%d"), '%Y')
      res$year <- as.numeric(res$year)
      
      #subsetting to the Caribbean area
      temp <- res[which(res$longitude > -92.8929 & res$longitude < -59.2015 & res$latitude < 27.303 & res$latitude > 8.2679),]
      
      temp2 <- temp[which(temp$year > 1987),] #removing all points older than 1988 (outside the range of data for KGC)
      temp3 <- temp2 %>% distinct(longitude, latitude, date, .keep_all = TRUE) #removing duplicates
      
      if(length(temp3$name > 0)){
        write.csv(temp3, paste0("plant_point_",list_absent_plant_zoo[i], ".csv"))
        
        point <- st_as_sf(x = temp3, 
                          coords = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        st_write(point, paste0("point_",list_absent_plant_zoo[i], ".shp"))
      } else{
        print(paste0(list_absent_plant_zoo[i], " not enough points"))
      }
    }, error=function(e){})} else{
      print(paste0(list_absent_plant_zoo[i], " not enough points"))
    }}
