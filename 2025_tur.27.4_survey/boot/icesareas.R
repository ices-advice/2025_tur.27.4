# icesareas.R - Downloads ICES areas shapefiles
# 2024_sol.27.4_benchmark_survey/boot/icesareas.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


filename <- "ICES_areas.zip"

download(paste0("http://gis.ices.dk/shapefiles/", filename))

unzip(filename)

unlink(filename)

