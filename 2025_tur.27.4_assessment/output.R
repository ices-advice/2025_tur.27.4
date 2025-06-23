## Load model, save pdf with model diagnostics, configure and save stock object for MIXFISH
# output.R - DESC
# 2025_tur.27.4_benchmark/output.R

# Copyright (c) WUR, 2025.
# Authors: Justin TIANO (WMR) <justin.tiano@wur.nl>
#          Lennert van de Pol (WMR) <lennert.vandepol@wur.nl>

# Load libraries in data.R or model.R

library(icesTAF)
mkdir("output")

# Run assessment if not done already (To obtain: TUR, TUR.sam, and TUR.retro, TUR.tun and/or TUR.Loo objects)
## Will take a few minutes
source.taf("model")

# Specify year
sens      <- "WGNSSK_2025"

### Load files (input TUR and output TUR.sam objects)
load("model/TUR_27.4_model.Rdata")

# Run model diagnostics

# Set up paths to directories 
run       <- "Final_"
assYear         <- 2025
codePath  <- paste("boot/initial/data/source/",sep="")
outPath   <- paste("output")

# Creates pdf with model diagnostics
library(data.table)
source("boot/initial/data/source/03b_runDiagnostics.r")

# Conversion for EQsim and MIXFISH 
## FLSAM to FLstock that can be run in EQsim

## Add data from input slots into FLSAM object
catch.wt2 <- exp(fit$pl$logCW)
catch.wt2 <- catch.wt2[, , 1]
catch.wt2 <- catch.wt2[1:length(1981:(assYear-1)),]
catch.wt3 <- t(catch.wt2)
catch.wt4 <- FLQuant(catch.wt3, dimnames=list(age=1:8, year=1981:2024), units="kg")

stock.wt2 <- exp(fit$pl$logSW)
stock.wt2 <- stock.wt2[1:length(1981:(assYear-1)),]
stock.wt3 <- t(stock.wt2)
stock.wt4 <- FLQuant(stock.wt3, dimnames=list(age=1:8, year=1981:2024), units="kg")

TUR.sam@stock.wt <- stock.wt4
TUR.sam@catch.wt <- catch.wt4
TUR@stock.wt <- stock.wt4
TUR@catch.wt <- catch.wt4
TUR.sam@mat      <- TUR@mat
TUR.sam@m        <- TUR@m

## Create stock (stk) object
stk <- TUR+TUR.sam 
catch.n(stk) <- exp(catch.n(TUR.sam)$log.mdl) # Needs 2024 data first 
catch(stk)   <- catch(TUR.sam)$value
landings.n(stk) <- catch.n(stk) 
discards.n(stk) <- 0

save(stk, file = "output/TUR.stockobject.Rda")
