# datras.R - GET DATRAS data for BTS Q3
# 2023_sol.27.4_survey/bootstrap/datras.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(data.table)

# SETTINGS

fy <- 1990:2023

# - DATRAS

library(DATRAS)

bts <- getDatrasExchange("BTS", years=fy, quarters=3, strict=FALSE)

sns <- getDatrasExchange("SNS", years=fy, quarters=3:4, strict=FALSE)

dyfs <- getDatrasExchange("DYFS", years=fy, quarters=3:4,  strict=FALSE)

# SAVE
save(bts, sns, dyfs, file=paste0("surveys_",
  format(Sys.Date(),  format="%Y%m%d"), ".rda"), compress="xz")

# - TODO: GET sweptarea
