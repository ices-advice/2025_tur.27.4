# model_indices.R - DESC
# 2024_sol.27.4_benchmark/model_indices.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(surveyIndex)
library(FLCore)
source('utilities.R')

library(doParallel)
mc.cores <- 4
registerDoParallel(mc.cores)

library(icesTAF)
mkdir("model")

# LOAD data

load("data/surveysAll.RData", verbose=TRUE)

load("data/grid.rda")

# SET dims and options

cutoff <- 0.1
cores <- 4

# SET models

modelsStatZ <- "Year + Gear + s(Ship, bs='re') +
  s(lon, lat, bs='ds', m=c(1, 0.5), k=kvecZ[a]) +
  s(lon, lat, bs='ds', m=c(1, 0.5), k=10, by=Year, id=1) +
  s(Depth,bs='ts',k=6) +
  offset(log(SweptArea))"

modelsStatP <- "Year + Gear + s(Ship, bs='re') +
  s(lon, lat, bs='ds', m=c(1, 0.5), k=kvecP[a]) +
  s(lon, lat, bs='ds', m=c(1, 0.5), k=5, by=Year, id=1) +
  s(Depth, bs='ts', k=6) +
  offset(log(SweptArea))"

# BTS.COAST surveys (combined) {{{

# SUBSET data
bts.coast <- subset(all, Survey %in% c('BTS','SNS','DYFS'))
xtabs(NoAtALK ~ Year+Age,data=bts.coast[[1]])
ages <- seq(1, 7)

# SET no. knots spatial splines
kvecP <- c(50, 50, 50, rep(80, length(ages)-3)) # 
# SET MAX dimension of smoother for each age group
kvecZ  <-  kvecP / 2 

# FIT Delta-Lognormal GAM aic = ; bic = 
gam_bts.coast <- getSurveyIdx(bts.coast, ages = ages,
  predD=nsgrid, cutOff = cutoff, 
  fam = rep("LogNormal", length(ages)),
  modelZ = rep(modelsStatZ, length(ages)),
  modelP = rep(modelsStatP, length(ages)),
  kvecP = kvecP, kvecZ = kvecZ,
  mc.cores = 1,
  predfix=list(Gear = "BT8", SweptArea = mean(bts.coast[[2]]$SweptArea)))

# BUILD index
BTS.COAST <- si2FLQuantPoint(gam_bts.coast)
bts.coast.idx <- si2FLIndex(gam_bts.coast) # convert to FLindex

# SAVE
save(bts.coast, gam_bts.coast, BTS.COAST, bts.coast.idx, nsgrid, file="model/gam/bts.coast.rda", compress="xz")       #lognormal

# Save as text file to copy to Lowestoft 'fleet.txt' file
surveyIndex::exportSI(x = gam_bts.coast$idx, ages = 1:7, years = 1991:2024, toy = 0.75, file = "output/BTS.COAST.txt")

# Plots 
plot(FLIndices(BTS.COAST=bts.coast.idx)) + facet_wrap(~age, ncol=2)
cohcorrplot(index(bts.coast.idx)) # doesn't work unless choosing only ages 1-5

# Internal consistency
internalCons(gam_bts.coast$idx)

# To load index files
load("model/gam/bts.coast.rda")    

# }}}

# BSAS - Industry survey {{{

# SUBSET data
bsas <- subset(all, Survey %in% c('NL-BSAS'))# & Year %in% c(2019:2023))
xtabs(NoAtALK ~ Year+Age,data=bsas[[1]]) 

# Substitute ages when data is missing using the mean from other years
for(aa in seq(0, 10)){ # takes a minute
  bsas <- fixAgeGroup(bsas, age=aa, n=3, fun=mean)
}

# Set ages and number of knots used
ages <- seq(1, 8)
kvecP <- c(50, 50, 50, rep(80, length(ages)-3)) 
kvecZ  <-  kvecP / 2 # SET MAX dimension of smoother for each age group

# take out gear (Model doesn't work with Gear since it is all the same) # add gear if combining indices
modZ <- "Year + s(Ship, bs = 're') + s(lon, lat, bs = 'ds', m=c(1, 0.5), k = kvecZ[a]) + 
s(lon, lat, bs = 'ds', m=c(1, 0.5), k = 10, by=Year, id=1) + s(Depth,bs='ts',k=6) + offset(log(SweptArea))"    
modP <- "Year + s(Ship, bs = 're') + s(lon, lat, bs = 'ds', m=c(1, 0.5), k = kvecP[a]) +  
s(lon, lat, bs = 'ds', m=c(1, 0.5), k = 10, by=Year, id=1) + s(Depth,bs='ts',k=6) + offset(log(SweptArea))"       

# FIT GAM

predfix=list(Gear = "BT12", SweptArea = mean(bsas[[2]]$SweptArea))

## Tweedie  aic = 5719.618; bic = 7141.351 best so far
model.tw <- modZ
system.time(gam_bsas <- getSurveyIdx(bsas, ages = ages,
                         predD= bsasgrid, cutOff = cutoff, 
                         fam = rep("Tweedie", length(ages)),
                         modelZ = rep(model.tw, length(ages)), 
                         modelP = rep(model.tw, length(ages)),
                         kvecP = kvecP, kvecZ = kvecZ,
                         mc.cores = 1,
                         predfix=predfix) 
            )

# BUILD index
BSAS <- si2FLQuantPoint(gam_bsas)
bsas.idx <- si2FLIndex(gam_bsas) # convert to FLindex

# SAVE
save(bsas, gam_bsas, BSAS, bsas.idx, bsasgrid, file="model/gam/bsas.rda", compress="xz")

# Save as text file to copy to Lowestoft 'fleet.txt' file
surveyIndex::exportSI(x = gam_bsas$idx, ages = 1:8, years = 2019:2024, toy = 0.75, file = "output/BSAS.txt")

# Plots 
plot(FLIndices(BSAS=bsas.idx)) + facet_wrap(~age, ncol=2)
cohcorrplot(index(bsas.idx)[1:6,]) # 
cohcorrplot(index(bsas.idx)[3:8,]) # 

# Internal consistency
internalCons(gam_bsas$idx)

# To load files if needed
load("model/gam/bsas.rda")    

# }}}


# Get survey with stratified mean if needed 
bts.coastSM <- getSurveyIdxStratMean(bts.coast, ageCols = 1:11)
dimnames(bts.coastSM) <- list(year = 1991:2024, age = 0:10)
bts.coastSM <- FLQuant(t(bts.coastSM), dimnames = list(year = ac(1991:2024), age = ac(0:10)))
bts.coastSM <- FLIndex(index = bts.coastSM)
