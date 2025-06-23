## Run analysis, write model results
# model.R - DESC
# 2025_tur.27.4_benchmark/model.R

# Copyright (c) WUR, 2025.
# Authors: Justin TIANO (WMR) <justin.tiano@wur.nl>
#          Lennert van de Pol (WMR) <lennert.vandepol@wur.nl>

library(icesTAF)
mkdir("model")

# Load libraries (see data.R script to install packages if needed)

library(FLCore);library(FLAssess);library(FLSAM);library(FLEDA); 
library(mgcv);library(splines);library(scales);library(gplots);
library(grid);library(gridExtra);library(latticeExtra); library(sas7bdat)
library(TMB);library(ggplot2); library(reshape2);
library(RColorBrewer);library(colorRamps)

# Load input data
load("data/input.RData")

###   Setup data structure for SAM assessment
### ------------------------------------------------------------------------------------------------------

# Data wrangling to get raw weights at age into SAM

#Replace -1 with NAs
stock@stock.wt[stock@stock.wt == -1.00] <- NA
stock@stock.wt[8, ] <- sapply(seq_along(stock@stock.wt[1, ]), function(i) {
  mean(stock@stock.wt[8:10, i], na.rm = TRUE)
})
stock@stock.wt[c(9:10),] <- stock@stock.wt[8,]

stock@catch.wt[stock@catch.wt == -1.00] <- NA
stock@catch.wt[8, ] <- sapply(seq_along(stock@catch.wt[1, ]), function(i) {
  mean(stock@catch.wt[8:10, i], na.rm = TRUE)
})
stock@catch.wt[c(9:10),] <- stock@catch.wt[8,]

stock@landings.wt[stock@landings.wt == -1.00] <- NA
stock@landings.wt[stock@landings.wt == -1.00] <- NA


stock@harvest.spwn[,2024-1975+1] <- 0
## Set up input and SAM control objects

TUR                         <- window(stock,start=1981)
TUR@catch.n[,ac(2000:2002)] <- -1                   # Raised UK data not trusted for North Sea
TUR@landings.n[]            <- TUR@catch.n
TUR                         <- setPlusGroup(TUR,8)  # 8+ plusgroup for catch

## Set up indices weighted by the inverse of their CV's
TUR.tun                     <- indices              # New model indices
TUR.ctrl                    <- FLSAM.control(TUR,TUR.tun)

#############################################################################
## special processing in WG2020, manually add 1.0133 t of sweden in area 4
## add on landing.n, landing, catch.n and catch

## 1. landing
landings_old <- TUR@landings[,"2019"]  
landings_new <- round(landings_old + 1.0133, digit=0)
TUR@landings[,"2019"]   <- landings_new
TUR@landings.n[,"2019"] <- as.vector(landings_new)/as.vector(landings_old)*as.vector(TUR@landings.n[,"2019"])
sum(TUR@landings.n[,"2019"]*TUR@landings.wt[,"2019"])

## catch and landings are equal for TUR
TUR@catch.n[,"2019"] <- as.vector(landings_new)/as.vector(landings_old)*as.vector(TUR@catch.n[,"2019"])

# check should be zero
TUR@landings.n[,"2019"] - TUR@catch.n[,"2019"]


### ------------------------------------------------------------------------------------------------------
# Set up model parameters
### ------------------------------------------------------------------------------------------------------

TUR.ctrl@states["catch unique",]            <- c(0,1,2,3,4,5,6,6)             # $keyLogFsta
TUR.ctrl@cor.F                              <- 2
TUR.ctrl@catchabilities["BTS.COAST",ac(1:7)]<- c(0,1,2,3,4,5,5)               # $keyLogFpar
TUR.ctrl@catchabilities["BSAS",ac(1:8)]     <- c(6,7,8,9,10,11,12,12)         #  
TUR.ctrl@f.vars["catch unique",]            <- c(0,1,2,2,3,3,3,3)             # $keyVarF
TUR.ctrl@logN.vars[]                        <- c(0,rep(1,7))                  # $keyVarLogN
TUR.ctrl@obs.vars["catch unique",]          <- c(0,1,1,1,1,1,2,2)       + 101 # $keyVarObs
TUR.ctrl@obs.vars["BTS.COAST",ac(1:7)]      <- c(0,1,1,1,1,2,2)         + 201 # 
TUR.ctrl@obs.vars["BSAS",ac(1:8)]           <- c(0,1,1,1,1,1,1,1)       + 301 # 
TUR.ctrl@cor.obs[]                          <- NA
TUR.ctrl@cor.obs["BTS.COAST",1:7]               <- rep(0,7)                   # 
TUR.ctrl@cor.obs.Flag[2]                    <- af("AR")
TUR.ctrl                                    <- update(TUR.ctrl)


# Convert from FLSAM to stockassessment SAM
library(stockassessment)
data <- FLSAM2SAM(FLStocks(residual=TUR),TUR.tun) # Input data
conf <- ctrl2conf(TUR.ctrl, data)                 # Control/Configuration

#Optional: save model input
#save(data,conf,fit,file = "stockassessment_input_07.RData")

#Set stock and catch weight model 
data$stockMeanWeight[is.nan(data$stockMeanWeight)] <- NA
data$catchMeanWeight[is.nan(data$catchMeanWeight)] <- NA
data$landMeanWeight[is.na(data$landMeanWeight)] <- 1
conf$stockWeightModel                       <- 1
conf$catchWeightModel                       <- 1
conf$keyStockWeightMean                     <- c(0,1,2,3,4,5,6,7)
conf$keyCatchWeightMean[1,]                 <- c(0,1,2,3,4,5,6,7)
conf$keyStockWeightObsVar                   <- c(0,0,0,0,0,0,0,0)
conf$keyCatchWeightObsVar[1,]               <- c(0,0,0,0,0,0,0,0)

# Set parameters
par <- stockassessment::defpar(data,conf)         

# Run assessment model
fit <- stockassessment::sam.fit(data,conf,par)    

#Simstudy if needed
#sim.fit <- simstudy(fit, 35)

#Extract weights and visualize if needed
#exp(fit$pl$logSW) # see data
#matplot(exp(fit$pl$logSW), type = "b")

#Convert to FLSAM object for plotting
TUR.sam <- SAM2FLR(fit,TUR.ctrl)

### ------------------------------------------------------------------------------------------------------
###   Run assessment
### ------------------------------------------------------------------------------------------------------
save(TUR,TUR.tun,TUR.ctrl,data,conf,par,
     file = "model/TUR_27.4_input_data.Rdata")
#load("model/TUR_27.4_input_data.Rdata")                          # load data if needed
#TUR.sam             <- FLSAM(TUR,TUR.tun,TUR.ctrl)               # prevous code to begin assessment in FLSAM
TUR.ctrl@residuals  <- FALSE; TUR.sam@control@residuals <- FALSE

# Run retrospective with 3 years due to BSAS starting in 2019 (benchmark 2025)
## Change to 5 years when possible (2027 possibly)
TUR.retro           <- stockassessment::retro(fit,year=5)

# Leave one out assessment
TUR.Loo <- stockassessment::leaveout(fit = fit)
plot(TUR.Loo)

# FLSAM leave one out
TUR.LooFL <- loo(TUR,TUR.tun,TUR.ctrl) # doesn't work well due to not enough BSAS years
plot(TUR.LooFL)

# Save
save(TUR, TUR.tun, TUR.ctrl,TUR.sam, TUR.retro, TUR.LooFL, fit, conf, file = "model/TUR_27.4_model.Rdata")

# Check runs
plot(TUR.sam)
plot(TUR.retro)
plot(TUR.LooFL)

# check numbers
ssb(TUR.sam)
fbar(TUR.sam)
rec(TUR.sam)

# 3 year mean mohns rho in percentages
stockassessment::mohn(TUR.retro)

# Some model diagnostics if needed
#log likelihood and AIC
fit
AIC(fit)
#simstudy(fit, nsim = 10) # simulation to see if they all converge