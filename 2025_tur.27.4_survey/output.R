# output.R - DESC
# /home/mosqu003/Active/sol.27.4_benchmark_2024/2024_sol.27.4_benchmark_survey/output.R

# Copyright (c) WUR, 2024.
# Author: Justin TIANO (WMR) <justin.tiano@wur.nl>
#         Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("output")

library(FLCore)

source("utilities.R")

# LOAD GAM results

bts.coast <- mget(load("model/gam/bts.coast.rda")) # load from external hard drive
bsas <- mget(load("model/gam/bsas.rda"))

# CREATE indices at age
indices <- FLIndices(
  BTS.COAST=FLIndex(index=mean(bts.coast$BTS.COAST), type='numbers'),
  BSAS=FLIndex(index=mean(bsas$BSAS), type='numbers'))

# COMPUTE CVs
cvsindices <- FLQuants(
  BTS.COAST=FLQuant(t(gam_bts.coast$idx.CV), dimnames=list(age=1:7, year=1991:2024)),
  BSAS=FLQuant(t(gam_bsas$idx.CV), dimnames=list(age=1:8, year=2019:2024))
)

# SAVE
save(indices, cvsindices, file="output/indices.rda", compress="xz")

# Load indices if needed
load("output/indices.rda")



