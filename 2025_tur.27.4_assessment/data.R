# data.R - DESC
# 2025_tur.27.4_benchmark/data.R

# Copyright (c) WUR, 2025.
# Authors: Justin TIANO (WMR) <justin.tiano@wur.nl> and
#          Lennert van de Pol (WMR) <lennert.vandepol@wur.nl>

# Install packages if needed

# install.packages(pkgs="FLAssess",repos="http://flr-project.org/R")
# install.packages(pkgs="FLEDA",repos="http://flr-project.org/R")
# install.packages(pkgs="FLCore",repos="http://flr-project.org/R")
# install.packages("FLSAM", repos="https://flr.r-universe.dev")
# install.packages("ggplotFL", repos="http://flr-project.org/R")
# devtools::install_github("fishfollower/SAM/stockassessment", ref="components")
# devtools::install_github("flr/FLSAM")

library(icesTAF)
library(FLCore);library(FLAssess);library(FLSAM);library(FLEDA); 
library(mgcv)
library(splines);
library(scales);
library(gplots);
library(grid);
library(gridExtra);
library(latticeExtra)
library(sas7bdat)
library(TMB);
library(ggplot2);
library(reshape2);
library(RColorBrewer);
library(colorRamps);

library(icesTAF)
mkdir("data")

## Assessment settings
## Stock name
stk <- "Turbot"
stk1 <- "TUR"
stock_Name      <- "tur-nsea"
run       <- "Final_"
sens      <- "WGNSSK_2025"

# Year (= year when assessment conducted.  i.e. have data up to assYear-1)
assYear         <- 2025
retroYr         <- 2025
endYear         <- min(assYear,retroYr)-1
Units           <- c("tonnes","thousands","kg")     # totals (product of the next two), numbers, weights-at-age
maxA            <- 10
pGrp            <- T
minFbar         <- 2
maxFbar         <- 6

# load input data
source("boot/initial/input_data.R")

#Change one point in weights-at-age of stock (2003, age 8)
stock@stock.wt[8,(2003-1975)+1] <- -1.00

# Save input in data folder
save(indices, stock, cvsindices, file = "data/input.RData")
