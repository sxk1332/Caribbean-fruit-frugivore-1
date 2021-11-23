### Analysis

setwd("../Data")
dat <- read.csv("clean_data.csv")

#setup
rownames(dat) <- dat[,1]
dat[,1] <- NULL

dat$phz.median <- as.factor(dat$phz.median)
dat$kgc.median <- as.factor(dat$kgc.median)

dat$median_soil <- ifelse(dat$median_soil == 0, "NA", dat$median_soil)
dat$median_soil <- as.factor(dat$median_soil)

dat$abiotic_plant <- as.numeric(dat$abiotic_plant)
dat$zoo_plant <- as.numeric(dat$zoo_plant)
dat$total.plant <- as.numeric(dat$total.plant)

dat$median_soil <- NULL  #median soil has a lot of missing data. 

#Getting rid of std columns
dat$elevation.std <- NULL
dat$precipitation.std <- NULL

#Making all mass effect of NA to 0.
dat$mass_effect[is.na(dat$mass_effect)] <- 0

#Removing all islands without reptile information or have fewer than 0 plant species.
dat <- dat[which(dat$total.plant > 0),] 

#### Correlation matrix: https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
# Removing categorical variables

temp_dat <- dat
temp_dat$Region <- NULL
temp_dat$kgc.median <- NULL
temp_dat$phz.median <- NULL

dat.cor = cor(temp_dat)
#write.csv(dat.cor, "correlation_matrix.csv")

####################
dat$pzoo <- dat$zoo_plant/dat$total.plant
dat$pabiotic <- dat$abiotic_plant/dat$total.plant

plot(dat$Area.km2, dat$total.plant)
plot(dat$Area.km2, dat$pzoo)


plot(dat$Area.km2, dat$pabiotic, pch=16, las=1, xlab = "Area (10,000 km^2)", ylab = "% zoochoric", xaxt = "n", yaxt = "n")
axis(1, seq(0, 110000, by=10000), seq(0,11, by=1))
axis(2, seq(0, 1, by =0.1), seq(0, 100, by =10), las=1)

plot(dat$Area.km2, dat$total.plant, pch=16, las=1, xlab = "Area (10,000 km^2)", ylab = "# Plant species", xaxt = "n", yaxt = "n")
axis(1, seq(0, 110000, by=10000), seq(0,11, by=1))
axis(2, seq(0, 3000, by =500), seq(0, 3000, by =500), las=1)


################ ABIOTIC PLANTS #########################
dat1 <- dat
dat1$zoo_plant <- NULL
dat1$total.plant <- NULL

########### TOTAL MODEL ################

dat_total <- dat1[,c(1,3,4,25,10,12,13,14,15,16,17,20,22)]

library(MASS)
# Fit the full model 
fit_total <- glm(abiotic_plant ~., data=dat_total, na.action = na.fail)
summary(fit_total)

########## ABIOTIC ONLY (keeping kgc.variety, min_temp_mean, precipitation_mean, and soil_variety)
dat_abiotic <- dat1[,c(3, 4, 25, 10, 12)]

# Fit the full model 
fit_total_abiotic <- glm(abiotic_plant ~., data=dat_abiotic, na.action = na.fail)
summary(fit_total_abiotic)

########## BIOTIC ONLY (keeping native.total.bird, native.total.reptile, total_bird_mass, and total_reptile_mass)
dat_biotic <- dat1[,c(3, 16, 17, 20, 22)]
fit_total_biotic <- glm(abiotic_plant ~., data=dat_biotic, na.action = na.fail)
summary(fit_total_biotic)

########## NEUTRAL ONLY (keeping Area.km2, near_length, number_near, and mass_effect)
dat_neutral <- dat1[,c(3, 1, 13, 14, 15)]
fit_total_neutral <- glm(abiotic_plant ~., data=dat_neutral, na.action = na.fail)
summary(fit_total_neutral)

########## ABIOTIC and BIOTIC 
dat_AB <- dat1[,c(3,4,25,10,12,16,17,20,22)]
fit_total_AB <- glm(abiotic_plant ~., data=dat_AB, na.action = na.fail)
summary(fit_total_AB)

########## ABIOTIC and NEUTRAL 
dat_AN <- dat1[,c(1,3,4,25,10,12,13,14,15)]
fit_total_AN <- glm(abiotic_plant ~., data=dat_AN, na.action = na.fail)
summary(fit_total_AN)

########## BIOTIC and NEUTRAL 
dat_BN <- dat1[,c(1,3,13, 14,15,16,17,20,22)]
fit_total_BN <- glm(abiotic_plant ~., data=dat_BN, na.action = na.fail)
summary(fit_total_BN)


########################## COMPARING MODEL AVERAGES #############
### MODEL AVERAGING
#Getting all models
library(MuMIn)
aic2 <- AIC(fit_total, 
            fit_total_abiotic, fit_total_biotic, fit_total_neutral,
            fit_total_AB, fit_total_AN, fit_total_BN)  

model.sel(fit_total, 
          fit_total_abiotic, fit_total_biotic, fit_total_neutral,
          fit_total_AB, fit_total_AN, fit_total_BN)

### Interpretation
summary(fit_total_BN)
summary(fit_total_biotic)

#install.packages('relaimpo')
library(relaimpo)
a1 <- calc.relimp(fit_total_BN, rela = TRUE)
a2 <- as.data.frame(a1@lmg)

b1 <- calc.relimp(fit_total_biotic, rela = TRUE)
b2 <- as.data.frame(b1@lmg)

################ ZOO PLANTS #########################
dat1 <- dat
dat1$abiotic_plant <- NULL
dat1$total.plant <- NULL

########### TOTAL MODEL ################

dat_total <- dat1[,c(1,3,4,25,10,12,13,14,15,16,17,20,22)]

library(MASS)
# Fit the full model 
fit_total <- glm(zoo_plant ~., data=dat_total, na.action = na.fail)
summary(fit_total)

########## ABIOTIC ONLY (keeping kgc.variety, min_temp_mean, precipitation_mean, and soil_variety)
dat_abiotic <- dat1[,c(3, 4, 25, 10, 12)]

# Fit the full model 
fit_total_abiotic <- glm(zoo_plant ~., data=dat_abiotic, na.action = na.fail)
summary(fit_total_abiotic)

########## BIOTIC ONLY (keeping native.total.bird, native.total.reptile, total_bird_mass, and total_reptile_mass)
dat_biotic <- dat1[,c(3, 16, 17, 20, 22)]
fit_total_biotic <- glm(zoo_plant ~., data=dat_biotic, na.action = na.fail)
summary(fit_total_biotic)

########## NEUTRAL ONLY (keeping Area.km2, near_length, number_near, and mass_effect)
dat_neutral <- dat1[,c(3, 1, 13, 14, 15)]
fit_total_neutral <- glm(zoo_plant ~., data=dat_neutral, na.action = na.fail)
summary(fit_total_neutral)

########## ABIOTIC and BIOTIC 
dat_AB <- dat1[,c(3,4,25,10,12,16,17,20,22)]
fit_total_AB <- glm(zoo_plant ~., data=dat_AB, na.action = na.fail)
summary(fit_total_AB)

########## ABIOTIC and NEUTRAL 
dat_AN <- dat1[,c(1,3,4,25,10,12,13,14,15)]
fit_total_AN <- glm(zoo_plant ~., data=dat_AN, na.action = na.fail)
summary(fit_total_AN)

########## BIOTIC and NEUTRAL 
dat_BN <- dat1[,c(1,3,13, 14,15,16,17,20,22)]
fit_total_BN <- glm(zoo_plant ~., data=dat_BN, na.action = na.fail)
summary(fit_total_BN)


########################## COMPARING MODEL AVERAGES #############
### MODEL AVERAGING
#Getting all models
library(MuMIn)
aic2 <- AIC(fit_total, 
            fit_total_abiotic, fit_total_biotic, fit_total_neutral,
            fit_total_AB, fit_total_AN, fit_total_BN)  

model.sel(fit_total, 
          fit_total_abiotic, fit_total_biotic, fit_total_neutral,
          fit_total_AB, fit_total_AN, fit_total_BN)

### Interpretation
summary(fit_total)
summary(fit_total_BN)
summary(fit_total_AB)

#install.packages('relaimpo')
library(relaimpo)
a1 <- calc.relimp(fit_total_BN, rela = TRUE)
a2 <- as.data.frame(a1@lmg)

b1 <- calc.relimp(fit_total, rela = TRUE)
b2 <- as.data.frame(b1@lmg)

c1 <- calc.relimp(fit_total_AB, rela = TRUE)
c2 <- as.data.frame(c1@lmg)

################ TOTAL PLANTS #########################
dat1 <- dat
dat1$abiotic_plant <- NULL
dat1$zoo_plant <- NULL

########### TOTAL MODEL ################

dat_total <- dat1[,c(1,3,4,25,10,12,13,14,15,16,17,20,22)]

library(MASS)
# Fit the full model 
fit_total <- glm(total.plant ~., data=dat_total, na.action = na.fail)
summary(fit_total)

########## ABIOTIC ONLY (keeping kgc.variety, min_temp_mean, precipitation_mean, and soil_variety)
dat_abiotic <- dat1[,c(3, 4, 25, 10, 12)]

# Fit the full model 
fit_total_abiotic <- glm(total.plant ~., data=dat_abiotic, na.action = na.fail)
summary(fit_total_abiotic)

########## BIOTIC ONLY (keeping native.total.bird, native.total.reptile, total_bird_mass, and total_reptile_mass)
dat_biotic <- dat1[,c(3, 16, 17, 20, 22)]
fit_total_biotic <- glm(total.plant ~., data=dat_biotic, na.action = na.fail)
summary(fit_total_biotic)

########## NEUTRAL ONLY (keeping Area.km2, near_length, number_near, and mass_effect)
dat_neutral <- dat1[,c(3, 1, 13, 14, 15)]
fit_total_neutral <- glm(total.plant ~., data=dat_neutral, na.action = na.fail)
summary(fit_total_neutral)

########## ABIOTIC and BIOTIC 
dat_AB <- dat1[,c(3,4,25,10,12,16,17,20,22)]
fit_total_AB <- glm(total.plant ~., data=dat_AB, na.action = na.fail)
summary(fit_total_AB)

########## ABIOTIC and NEUTRAL 
dat_AN <- dat1[,c(1,3,4,25,10,12,13,14,15)]
fit_total_AN <- glm(total.plant ~., data=dat_AN, na.action = na.fail)
summary(fit_total_AN)

########## BIOTIC and NEUTRAL 
dat_BN <- dat1[,c(1,3,13, 14,15,16,17,20,22)]
fit_total_BN <- glm(total.plant ~., data=dat_BN, na.action = na.fail)
summary(fit_total_BN)


########################## COMPARING MODEL AVERAGES #############
### MODEL AVERAGING
#Getting all models
library(MuMIn)
aic2 <- AIC(fit_total, 
            fit_total_abiotic, fit_total_biotic, fit_total_neutral,
            fit_total_AB, fit_total_AN, fit_total_BN)  

model.sel(fit_total, 
          fit_total_abiotic, fit_total_biotic, fit_total_neutral,
          fit_total_AB, fit_total_AN, fit_total_BN)

### Interpretation
# The global model and the BN model are equally supported from data
summary(fit_total_BN)
summary(fit_total)
summary(fit_total_biotic)

#install.packages('relaimpo')
library(relaimpo)
a1 <- calc.relimp(fit_total_BN, rela = TRUE)
a2 <- as.data.frame(a1@lmg)

b1 <- calc.relimp(fit_total, rela = TRUE)
b2 <- as.data.frame(b1@lmg)

c1 <- calc.relimp(fit_total_biotic, rela = TRUE)
c2 <- as.data.frame(c1@lmg)