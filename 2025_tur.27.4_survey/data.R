# data.R - DESC
# 2024_sol.27.4_benchmark/data.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(icesTAF)
mkdir("data")

library(data.table)
library(icesVocab)
library(DATRAS)

source("utilities.R")

# Make sure to run taf.boot() (and now things take a few minutes to download everything)

# Code to update from previous year

## Download data from DATRAS

qrs <- seq(1, 4)

bsas24 <- getDatrasExchange("NL-BSAS", years = 2024, quarters = qrs, strict = FALSE)
bts24 <- getDatrasExchange("BTS", years = 2024, quarters = qrs, strict = FALSE)
sns24 <- getDatrasExchange("SNS", years = 2024, quarters = qrs, strict = FALSE)
dyfs24 <- getDatrasExchange("DYFS", years = 2024, quarters = qrs, strict = FALSE)

# ADD spatial data
bsas24 <- addSpatialData(bsas24, file.path("boot", "data", "shapefiles","ICES_areas.shp"))
sns24 <- addSpatialData(sns24, file.path("boot", "data", "shapefiles","ICES_areas.shp"))
bts24 <- addSpatialData(bts24, file.path("boot", "data", "shapefiles","ICES_areas.shp"))
dyfs24 <- addSpatialData(dyfs24, file.path("boot", "data", "shapefiles","ICES_areas.shp"))

d24 <- c(bsas24, sns24, dyfs24, bts24)
d24 <- subset(d24, ICESAREA %in% c("IVa", "IVb", "IVc") | ICES_SUB %in% c("IVa", "IVb", "IVc"))

# SELECT SpecVal 1 (valid), 10 (no weight)
d24 <- subset(d24, SpecVal %in% c(1, 10))
d24 <- DATRAS:::fixMissingHaulIds(d24, strict = TRUE)

# Save all surveys before swept area calculations
save(d24, file="data/2024BTsurveys.RData", compress="xz") 

# SUBSET turbot (127149)
id <- findAphia("Scophthalmus maximus", latin=TRUE)
d24 <- subset(d24, Valid_Aphia == id) 

# Combine all beam trawl surveys
all <- d24

# Prepare for swept area calculation

## Calculate beam width for single and double beams

all[["HH"]]$Beamwidth <- as.numeric(ifelse(
  all[["HH"]]$Gear == "BT12", 
  substr(all[["HH"]]$Gear, start = 3, stop = 4),  # For "BT12", use characters 3 and 4
  substr(all[["HH"]]$Gear, start = 3, stop = 3)   # For others, use character 3
))
unique(all[["HH"]]$Gear) # to check gears

### Fix for double beams (* 2)
all[["HH"]]$Beamwidth[ !is.na(all[["HH"]]$GearEx) & all[["HH"]]$GearEx=="DB" ] <- 
  all[["HH"]]$Beamwidth[ !is.na(all[["HH"]]$GearEx) & all[["HH"]]$GearEx=="DB" ]*2

unique(all[["HH"]]$Beamwidth) # to check gear widths

## Subset different surveys for SA calculations
bts24  <- subset(all, Survey %in% c('BTS'))
sns24  <- subset(all, Survey %in% c('SNS'))
dyfs24 <- subset(all, Survey %in% c('DYFS'))
bsas24  <- subset(all, Survey %in% c('NL-BSAS'))

#############################################################################
# -- SWEPT AREA CALCULATIONS  
#############################################################################

## - BTS

hh <- data.table(bts24[['HH']])

# CALCULATE Sweptarea per km squared
## Here we choose to trust the 'Distance' given in the BTS but if NA, we derive distance from the GroundSpeed (kn) * HaulDur. 
## If GroundSpeed==NA, we choose 4 kn ('normal' BTS speed)

hh[, DeriveDistance := ifelse(!is.na(Distance), Distance, (ifelse(is.na(GroundSpeed), 4, GroundSpeed) * 1853 * HaulDur) / 60)]
hh[Country %in% c('NL', 'BE'), SweptArea := (DeriveDistance * Beamwidth) / 1e6]
hh[Country == 'DE', SweptArea := (DeriveDistance * Beamwidth) / 1e6]
hh[Country == 'GB', SweptArea := 4 * 1.852 * (HaulDur / 60) * (4 / 1000)]

# Check data for NA's and mean values
hh[, .(NA_Count = sum(is.na(SweptArea))), by = Country] # 
hh[, .(SA = mean(SweptArea)), by = Country] # 
hh[, .(DB = sum(GearEx=="DB")), by = Country] #
hh[, .(Gear = unique(Gear)), by = Country] #

# REASSIGN to bts
bts24[['HH']] <- as.data.frame(hh)


## - SNS

hh <- data.table(sns24[['HH']])

# CALCULATE Sweptarea per km squared
#hh[, DeriveDistance := ((ifelse(is.na(GroundSpeed), mean(hh$GroundSpeed, na.rm = T), GroundSpeed) * 1853 * HaulDur) / 60)] # if we don't trust distance measured use derived distance
hh[, DeriveDistance := ifelse(!is.na(Distance), Distance, 
                              (ifelse(is.na(GroundSpeed), mean(hh$GroundSpeed, na.rm = T), GroundSpeed) * 1853 * HaulDur) / 60)]
hh[, SweptArea := (DeriveDistance * Beamwidth) / 1e6]

# Check data for NA's and mean values
hh[, .(NA_Count = sum(is.na(SweptArea))), by = Country] # 
hh[, .(SA = mean(SweptArea)), by = Country] # 
hh[, .(DB = sum(GearEx=="DB")), by = Country] #
hh[, .(Gear = unique(Gear)), by = Country] #

# REASSIGN to sns
sns24[['HH']] <- as.data.frame(hh)


## - DYFS

hh <- data.table(dyfs24[['HH']])

# CALCULATE Sweptarea per km squared
hh[, DeriveDistance := ifelse(!is.na(Distance), Distance, (ifelse(is.na(GroundSpeed), mean(hh$GroundSpeed, na.rm = T), GroundSpeed) * 1853 * HaulDur) / 60)]
hh[, SweptArea := (DeriveDistance * Beamwidth) / 1e6]

# Check data for NA's and mean values
hh[, .(NA_Count = sum(is.na(SweptArea))), by = Country] # 
hh[, .(SA = mean(SweptArea)), by = Country]             # Swept area per country
hh[, .(DB = sum(GearEx=="DB")), by = Country] # Double beam
hh[, .(Gear = unique(Gear)), by = Country] #

# REASSIGN to dyfs
dyfs24[['HH']] <- as.data.frame(hh)

sum(is.na(dyfs[['HH']]$SweptArea)) # check NA's


## - BSAS

## Here we do not trust the inputed distance and choose to use the calculated DerivedDistance for SweptArea calculations

hh <- data.table(bsas24[['HH']])

# CALCULATE Sweptarea per km squared
## GroundSpeed is available for all hauls in BSAS
hh[, DeriveDistance := ((GroundSpeed * 1853 * HaulDur) / 60)]
hh[, SweptArea := (DeriveDistance * Beamwidth)/ 1e6] 

# Check data for NA's and mean values
hh[, .(tot_distance = sum(DeriveDistance)), by = Year] # to check differences per year
hh[, .(totSA = sum(SweptArea)), by = Year]             # to check differences per year
hh[, .(NA_Count = sum(is.na(SweptArea)))]              # NA check
hh[, .(SA = mean(SweptArea))]                          # SA mean
hh[, .(DB = sum(GearEx=="DB"))]                        # Double beam; should be all of the hauls
hh[, .(Gear = unique(Gear))]                           # Gear check: should be BT12

bsas24[['HH']] <- as.data.frame(hh)

# Fix AgeGroup in surveyIndex

# Recombine all data with SA calculation per survey
all <- c(bsas24, bts24, sns24, dyfs24)

library(surveyIndex)

all <- removeZeroClusters(all)

# ADD length spectrum
all <- addSpectrum(all, cm.breaks=seq(0, 70, by=1))

# Substitute ages when data is missing using the mean from other years
for(aa in seq(0, 10)){ # takes a minute
  all <- fixAgeGroup(all, age=aa, n=3, fun=mean)
} 

# Save processed data with SA
save(all, file="data/TUR_surveys2024_SA.RData", compress="xz")

###########################################
# GET CATCH NUMBERS AT AGE FOR 2024 DATA
##########################################

# Check for missing data
xtabs(NoAtALK ~ Year+Age,data=all[["CA"]])  

# To speed things up
registerDoParallel(3)

# SET Nage in HH data (can take a while to run)
## Need to only include survey years where CA data is available or it will crash
all <- setNage(all, ages=c(0, 10))

# SUBSET for HaulVal, HaulDur and SweptArea to use in CPUE
all <- subset(all, HaulDur > 4 & HaulVal == "V" & !is.na(SweptArea))

save(all, file="data/surveys2024.RData", compress="xz")

#########################################
# Merge 2024 data with historical data
#########################################

## Load 2024 data with catch numbers at age
load("data/surveys2024.RData")
all24 <- all 

## Load historical survey data (1991-2023)
load("boot/initial/data/surveys/surveys.rda")

# merge 2024 data with the rest 
all <- c(all24, all) 

# Fix Ntot column in HH data
Nage <- all[["HH"]]$Nage

# Recalculate Ntot
Ntot <- rowSums(Nage)
sum(is.na(Ntot)) # check for NAs

all[["HH"]]$Ntot <- Ntot

# Save merged data file
save(all, file="data/surveysAll.RData", compress="xz")

# If you want to try to source the grid
# source('data_grid.R')
