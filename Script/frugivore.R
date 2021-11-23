### Frugivore ####

setwd("../Data/BIOTIC")
dat <- read.csv("frugivore_data.csv")

#native
frugivore_dat <- dat[which(dat$Frugivore == 1 & dat$Status == 1),]

#bird
bird_dat <- dat[which(dat$Taxa == "bird" & dat$Status ==1),]
bird_dat$Body.mass <- as.numeric(bird_dat$Body.mass)

## Supp. Figure 2
boxplot(bird_dat$Body.mass, las=1, ylab="body mass (kg)", xlab="Bird body mass", yaxt = "n", ylim = c(0,1000), col=c("white","grey"), outline= FALSE)
axis(2, seq(0,1000, by = 100), seq(0, 1, by = 0.1), las=1)
points(jitter(rep(1, length(bird_dat$Body.mass)), f=4), bird_dat$Body.mass, col="grey48", pch=16)

#reptile
reptile_dat <- dat[which(dat$Taxa == "reptile" & dat$Status ==1),]
reptile_dat$Body.mass <- as.numeric(reptile_dat$Body.mass)

## Supp. Figure 3
boxplot(reptile_dat$Body.mass, las=1, ylab="body mass (kg)", xlab="Reptile body mass", yaxt = "n", ylim = c(0,20000), col=c("white","grey"), outline= FALSE)
axis(2, seq(0,20000, by = 1000), seq(0, 20, by = 1), las=1)
points(jitter(rep(1, length(reptile_dat$Body.mass)), f=4), reptile_dat$Body.mass, col="grey48", pch=16)

####################################################
##################  BIRD ###########################
####################################################

bird_dat <- frugivore_dat[which(frugivore_dat$Taxa == "bird"),]
list_temp1 <- unique(bird_dat$Species)

length(unique(bird_dat$Family))

#data from birdlife
avail_bird_dat <- read.csv("bird_species.csv")
list_temp2 <- unique(avail_bird_dat$binomial)

#getting data available from birdlife
subset_bird_dat <- subset(bird_dat, Species %in% list_temp2)
list_temp3 <- unique(subset_bird_dat$Species)

#write.csv(subset_bird_dat, "subset_bird_dat.csv")

#getting data absent from frugivore dataset
absent_bird_list <- subset(list_temp1, !(list_temp1 %in% list_temp2))
absent_bird_dat <- subset(bird_dat, Species %in% absent_bird_list)

#write.csv(absent_bird_dat, "absent_bird_dat.csv")

### Getting point data for absent bird

library("spocc")
library("rgdal")
library("GISTools")
library("sf")

setwd("./bird_point")

for(i in c(1:length(absent_bird_list))){
  results <- occ(query = as.character(absent_bird_list[i]), from= c('gbif', 'bison', 'inat', 'vertnet'), limit = 1000)
  res <- occ2df(results)
  res$longitude <- as.numeric(res$longitude)
  res$latitude <- as.numeric(res$latitude)
  temp <- res[which(res$longitude > -92.8929 & res$longitude < -59.2015 & res$latitude < 27.303 & res$latitude > 8.2679),]
  print(paste0(absent_bird_list[i]))
  print(i)
  write.csv(temp, paste0("bird_absent_point_",absent_bird_list[i], ".csv"))
  
  if(length(temp$name > 0)){ 
    tryCatch({
      point <- st_as_sf(x = temp, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      st_write(point, paste0("bird_absent_point_",absent_bird_list[i], ".shp"))
    }, error=function(e){})} else{
      print(paste0(absent_bird_list[i], " not enough points"))
    }}

####################################################
##################  REPTILE ###########################
####################################################

reptile_dat <- frugivore_dat[which(frugivore_dat$Taxa == "reptile"),]
list_temp1 <- unique(reptile_dat$Species)

length(unique(reptile_dat$Family))

#data from iucn
setwd("..")
avail_reptile_dat <- read.csv("REPTILES.csv")
list_temp2 <- unique(avail_reptile_dat$binomial)

#getting data available from iucn
subset_reptile_dat <- subset(reptile_dat, Species %in% list_temp2)
list_temp3 <- unique(subset_reptile_dat$Species)

#write.csv(subset_reptile_dat, "subset_reptile_dat.csv")

#getting data absent from frugivore dataset
absent_reptile_list <- subset(list_temp1, !(list_temp1 %in% list_temp2))
absent_reptile_dat <- subset(reptile_dat, Species %in% absent_reptile_list)

#write.csv(absent_reptile_dat, "absent_reptile_dat.csv")

### Getting point data for absent reptile
setwd("./reptile_point")

library("spocc")
library("rgdal")
library("GISTools")
library("sf")

for(i in c(1:length(absent_reptile_list))){
  results <- occ(query = as.character(absent_reptile_list[i]), from= c('gbif', 'bison', 'inat', 'vertnet'), limit = 1000)
  res <- occ2df(results)
  res$longitude <- as.numeric(res$longitude)
  res$latitude <- as.numeric(res$latitude)
  temp <- res[which(res$longitude > -92.8929 & res$longitude < -59.2015 & res$latitude < 27.303 & res$latitude > 8.2679),]
  temp$species <- absent_reptile_list[i]
  print(paste0(absent_reptile_list[i]))
  print(i)
  write.csv(temp, paste0("reptile_absent_point_",absent_reptile_list[i], ".csv"))
  
  if(length(temp$name > 0)){ 
    tryCatch({
      point <- st_as_sf(x = temp, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      st_write(point, paste0("reptile_absent_point_",absent_reptile_list[i], ".shp"))
    }, error=function(e){})} else{
      print(paste0(absent_reptile_list[i], " not enough points"))
    }}

############# ISLAND INFO #################
library(foreign)
library(tidyverse)
setwd("C:/Users/Seokmin/Desktop/Miami/Galetti projects/Caribbean_hotspots/Data/FRUGIVORE/total/compiled/islands")

list_dat <- list.files(setwd("C:/Users/Seokmin/Desktop/Miami/Galetti projects/Caribbean_hotspots/Data/FRUGIVORE/total/compiled/islands"), pattern= ".dbf")

list_temp <- str_remove(list_dat, ".tif_island.dbf")
list_frugivore <- str_replace(list_temp, "_", " ")

mat <- matrix(NA, nrow=length(list_frugivore), ncol=2)
# converting matrix to data frame
df_frug <- data.frame(mat)
df_frug$X1 <- NULL
df_frug$X2 <- NULL

df_frug$Species <- list_frugivore
df_frug$no_island <- as.numeric(0)

for(i in c(1:length(list_frugivore))){
  temp <- read.dbf(list_dat[i])
  df_frug$no_island[i] <- length(temp$caribbean1)
}

write.csv(df_frug, "frug_island.csv")
