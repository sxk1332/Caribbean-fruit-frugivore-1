### Island configuration

library(foreign)

setwd("../Data/ISLAND_BIOGEOGRAPHY/dbf")

# Getting list of plants to compare with reviewed fruit data
library(tidyverse)
list_dat <- list.files(getwd(), pattern= ".dbf")

list_temp <- str_remove(list_dat, "_NearTable.dbf")
list_island <- str_replace(list_temp, "_", " ")

df_island <- data.frame(id = 1:length(list_temp), stringsAsFactors = FALSE, 
                        species = as.character(list_island)
)
df_island$near_length <- NA
df_island$number_near <- NA

for(i in c(1:length(list_dat))){
  tryCatch({
    temp <- read.dbf(list_dat[i])
    df_island$near_length[i] <- temp$NEAR_DIST[2]
    df_island$number_near[i]<- length(temp$IN_FID)-1
  }, error=function(e){})} 

#write.csv(df_island, "island_near_info.csv")

### MASS EFFECT ########################
# Get areas for each of the islands.
setwd("..")
islandinfo <- read.csv("island_info.csv")
islandnear <- read.csv("island_near_info.csv")
islandnear$mass_effect <- NA

#Get what is in common between list in d1 and islandinfo
#Total islands: 
list_total <- unique(islandinfo$OID_)

setwd("./dbf")

for(i in c(1:length(list_dat))){
  tryCatch({
    d1 <- read.dbf(list_dat[i])
    list_temp <- unique(d1$NEAR_FID)
    subset_df <- subset(islandinfo, (list_total %in% list_temp))
    
    d_temp <- d1[order(d1$NEAR_FID),]
    d_temp2 <- cbind(d_temp, subset_df)
    
    name_temp <- str_remove(list_dat[i], "_NearTable.dbf")
    name_island <- str_replace(name_temp, "_", " ")
    
    d_temp2 <- d_temp2[which(d_temp2$caribbean1 != name_island),]  # Removing the identity island since we do not want to count itself.
    
    #There are some islands with nearest distance of 0, as their island polygons were touching. In these cases, our equation does not work as you cannot divide by 0.
    #Therefore, we substitute a small number (1 meter) to make the equation work.
    d_temp2$NEAR_DIST <- ifelse(d_temp2$NEAR_DIST == 0, 1, d_temp2$NEAR_DIST)  
    
    islandnear$mass_effect[i] <- sum((1/d_temp2$NEAR_DIST)*d_temp2$AREA_GEO) / (sum(1/d_temp2$NEAR_DIST))
  }, error=function(e){})} 

setwd("..")
#write.csv(islandnear, "island_near_info2.csv")
