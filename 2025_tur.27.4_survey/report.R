# report.R - DESC
# 2024_sol.27.4_benchmark_survey/report.R

# Copyright (c) WUR, 2024.
# Author: Justin TIANO (WMR) <justin.tiano@wur.nl>
#         Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#         
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("report")

library(ggplotFL)
library(patchwork)
library(grafify)
library(surveyIndex)
library(data.table)
source('utilities.R')

# setup {{{

# COMMON scales for survey or country

scale_survey <- scale_color_grafify(palette='fishy')
scale_survey_fill <- scale_fill_grafify(palette='fishy')
scale_country <- scale_color_grafify(palette='safe')
scale_country_fill <- scale_fill_grafify(palette='safe')

# NS map

nsd <- map_data("worldHires", xlim = c(-5, 15), ylim = c(45, 65))

nsm <- ggplot(nsd) + geom_polygon(aes(long, lat, group = group), fill="gray") +
  coord_quickmap(xlim = c(-3, 9.5), ylim = c(50.5, 59), expand = FALSE) +
  xlab("") + ylab("") + theme_bw()
# }}}

# -- grid {{{

load("data/grid.rda")

taf.png("grid_northsea.png", width=800)
nsm + geom_point(data=nsgrid[[1]], aes(x=lon, y=lat), alpha=0.2, size=1) +
  ggtitle("BTS+COAST grid")
dev.off()

taf.png("grid_bsas.png", width=800)
nsm + geom_point(data=bsasgrid, aes(x=lon, y=lat), alpha=0.2, size=1) +
  ggtitle("BSAS grid")
dev.off()

# }}}

# -- surveys {{{

load("data/surveysAll.RData", verbose=TRUE)

all <- addSpectrum(all, cm.breaks=seq(0, 70, by=1))
all <- addWeightByHaul(all, to1min=FALSE)

hh <- data.table(all[['HH']])
hl <- data.table(all[['HL']])
ca <- data.table(all[['CA']])

hh[, Density := HaulWgt / SweptArea / 1000]

# TODO: CHECK w/CC

id <- rowSums(hh[, .(Nage.0, Nage.1, Nage.2, Nage.3, Nage.4, Nage.5, Nage.6, Nage.7, Nage.8, Nage.9, Nage.10.)]) > 0

# - No. hauls year/country

dt <- hh[, .(Hauls=.N), by=.(Survey, Year, Country)]

taf.png("report/data_hauls_survey.png", width=1000)
ggplot(dt, aes(fill=Country, y=Hauls, x=ISOdate(Year,1,1))) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(Survey~.) + scale_country_fill +
  xlab("") + ylab("No. of hauls")
dev.off()

# - Hauls by survey

#taf.png("report/map_hauls_survey.png", width=1000)
#nsm + geom_point(data=hh, aes(x=ShootLong, y=ShootLat, colour=Survey),
#  alpha=0.2, size=0.5) + scale_survey + theme(legend.position='bottom') +
#  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8)))
#dev.off()

# Recreate with more distinct colors
distinct_colors <- scale_colour_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928"))

taf.png("report/map_hauls_survey2.png", width=1000)
nsm + geom_point(data=hh, aes(x=ShootLong, y=ShootLat, colour=Survey),
                 alpha=0.2, size=0.5) + distinct_colors + theme(legend.position='bottom') +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8)))
dev.off()

taf.png("report/map_hauls_survey_year.png", width=1000)
nsm + geom_point(data=hh[Year %in% c(seq(1990, 2020, by=5), 2021, 2023)],
  aes(x=ShootLong, y=ShootLat, colour=Survey), alpha=0.2, size=0.5) +
  scale_survey + theme(legend.position='bottom') +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8))) +
  facet_wrap(~Year)
dev.off()


# - Hauls by survey in 2024

taf.png("report/map_hauls_survey_2024.png", width=1000)
nsm + geom_point(data=hh[Year %in% 2024],
  aes(x=ShootLong, y=ShootLat, colour=Survey), alpha=0.8, size=0.5) +
  scale_survey + theme(legend.position='bottom') +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8))) +
  ggtitle('2024')
dev.off()

taf.png("report/map_hauls_survey_2024_turbot.png", width=1000)
nsm + geom_point(data=hh[Density > 0 & Year %in% 2024],
  aes(x=ShootLong, y=ShootLat, colour=Survey, size=Density), alpha=0.8) +
  scale_survey + theme(legend.position='bottom') +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8))) +
  ggtitle('2024')
dev.off()

# - BTS Hauls by Country

taf.png("report/map_hauls_bts.png")
nsm + geom_point(data=hh[Survey == 'BTS'],
  aes(x=ShootLong, y=ShootLat, colour=Country), alpha=0.5) +
  scale_country + theme(legend.position='bottom') +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8)))
dev.off()

# - BTS Hauls by Lustrum

yrs <- c(seq(1985, 2018, by=5), seq(2020, 2024))

taf.png("report/map_lustrum_bts.png")
nsm + geom_point(data=hh[Survey == 'BTS' & Year %in% yrs],
  size=0.6, alpha=0.6, aes(x=ShootLong, y=ShootLat, colour=Country)) +
  facet_wrap(~Year) +
  scale_country + theme(legend.position='bottom') +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.8)))
dev.off()

# - Hauls by Lustrum & Survey

taf.png("report/map_lustrum_surveys.png")
nsm + geom_point(data=hh[Year %in% yrs],
  size=0.6, alpha=0.6, aes(x=ShootLong, y=ShootLat, colour=Survey)) +
  facet_wrap(~Year) +
  scale_survey + theme(legend.position='bottom')
dev.off()

# - Hauls by Survey over last 4 years

taf.png("report/map_recent_surveys.png", width=800)
nsm + geom_point(data=hh[Year %in% seq(2021, 2024)],
  size=0.6, alpha=0.6, aes(x=ShootLong, y=ShootLat, colour=Survey)) +
  facet_wrap(~Year) +
  scale_survey + theme(legend.position='bottom')
dev.off()

# - No. individuals per year and country

dt <- ca[, .(Fish=.N), by=.(Year, Survey)]

taf.png("report/data_aged_survey.png")
ggplot(dt, aes(x=ISOdate(Year,1,1), y=Fish, fill=Survey)) + 
  geom_bar(stat="identity") +
  facet_grid(Survey~.) +
  xlab("") + ylab("No. of aged fish") +
  scale_survey_fill
dev.off()

# No. aged individuals ages 0-2 in BTS

dt <- ca[Age %in% seq(0, 2) & Survey == 'BTS', .(Fish=.N), by=.(Year, Age)]

taf.png("report/data_aged02_BTS.png")
ggplot(dt, aes(x=ISOdate(Year,1,1), y=Fish)) + 
  geom_bar(stat="identity") +
  facet_grid(Age~., 
    labeller=as_labeller(c('0'=0,'1'=1,'2'=2))) +
  xlab("") + ylab("No. of aged fish, ages 0-2")
dev.off()

# No. aged individuals ages 3-6 in BTS

dt <- ca[Age %in% seq(3, 6) & Survey == 'BTS', .(Fish=.N), by=.(Year, Age)]

taf.png("report/data_aged36_BTS.png")
ggplot(dt, aes(x=ISOdate(Year,1,1), y=Fish)) + 
  geom_bar(stat="identity") +
  facet_grid(Age~., 
    labeller=as_labeller(c('3'=3,'4'=4,'5'=5,'6'=6))) +
  xlab("") + ylab("No. of aged fish, ages 3-6")
dev.off()

# No. aged individuals ages 7-10 in BTS

dt <- ca[Age > 5 & Survey == 'BTS', .(Fish=.N), by=.(Year, Age)]
dt[Age >= 10, Age:=10]

taf.png("report/data_aged_BTS.png")
ggplot(dt, aes(x=ISOdate(Year,1,1), y=Fish)) + 
  geom_bar(stat="identity") +
  facet_grid(Age~., 
    labeller=as_labeller(c('6'=6,'7'=7,'8'=8,'9'=9,'10'='10+'))) +
  xlab("") + ylab("No. of aged fish, ages 6--10+")
dev.off()

# Length distributions per survey

dt <- hl[, .(Count=sum(Count, na.rm=TRUE)), by=.(Survey, sizeGroup)]
dt[, Sum:=sum(Count, na.rm=TRUE), by=Survey]
dt[, Prop:=Count/Sum]
dt <- dt[Count != 0,]

foo <- function(x) {
  as.numeric(gsub("\\D", "", unlist(lapply(strsplit(as.character(x), ','), '[[', 1))))
}
dt[, Length:=foo(sizeGroup)]

taf.png("report/data_lengths_surveys.png")
ggplot(dt, aes(x=Length, y=Prop, fill=Survey)) +
  geom_bar(position="dodge2", stat="identity") +
  xlab("") + ylab("Proportion") + xlim(c(0, 40)) +
  scale_survey_fill
dev.off()

# Sweptarea

taf.png("report/data_sweptarea_hauldur.png")
ggplot(hh, aes(x=HaulDur, y=SweptArea)) +
  geom_point(aes(color=Survey)) +
  scale_survey +
  ylim(c(0, 0.1))
dev.off()

# stock.wt

# }}}

# model {{{

# BTS

load("model/gam/bts.coast.rda") # 

taf.png("indices_bts.coast_fit")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=c("index"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bts.coast_residuals")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=c("residuals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bts.coast_resyear")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=c("resVsYear"), plotByAge=FALSE, legend=FALSE)
dev.off()

# Residual maps

taf.png("indices_bts.coast_res2021")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2021, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bts.coast_res2022")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2022, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bts.coast_res2023")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2023, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bts.coast_res2024")
surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2024, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

# Estimation maps

surveyIndex::surveyIdxPlots(gam_bts.coast, bts.coast, myids=nsgrid,
  par=list(mfrow= c(4,3), mar=c(4,1,1,1)), year=2022, map.cex=0.4, map.pch=21,
  select=c("map"), plotByAge=TRUE, legend=FALSE)


# BSAS

load('model/gam/bsas.rda')

taf.png("indices_bsas_fit")
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=c("index"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bsas_residuals")
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=c("residuals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bsas_resyear")
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=c("resVsYear"), plotByAge=FALSE, legend=FALSE)
dev.off()

surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)),
  select=1, plotByAge=FALSE, legend=FALSE)

# Residual maps

taf.png("indices_bsas_res2021") # 
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2021, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bsas_res2022")
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2022, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bsas_res2023") # 
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2023, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

taf.png("indices_bsas_res2024") # starts in 2019
surveyIndex::surveyIdxPlots(gam_bsas, bsas, myids=bsasgrid,
  par=list(mfrow= c(3,3), mar=c(4,1,1,1)), year=2024, map.cex=0.4, map.pch=21,
  select=c("spatialResiduals"), plotByAge=FALSE, legend=FALSE)
dev.off()

# }}}

# indices {{{

load('output/indices.rda')

taf.png("indices_bts.coast")
plot(indices$BTS.COAST) + facet_grid(age~., scales='free')
dev.off()

taf.png("indices_bsas")
plot(indices$BSAS) + facet_grid(age~., scales='free')
dev.off()

taf.png("indices", height=1500, width = 3000)
plot(FLIndices(
  BTS.COAST=indices$BTS.COAST,
  BSAS=indices$BSAS)) +
  facet_wrap(~age, ncol=2)
dev.off()

taf.png("indices_bts.coast_cohcorr", height=1600)
cohcorrplot(index(indices$BTS.COAST))
dev.off()

taf.png("indices_bts.coast_cohcorr_decades", width=3000)
cohcorrplot(index(indices$BTS.COAST)[, ac(1991:2007)]) +
cohcorrplot(index(indices$BTS.COAST)[, ac(2008:2024)])
dev.off()

taf.png("indices_bsas_cohcorr", width=3000)
cohcorrplot(index(indices$BSAS)[1:6,]) + 
cohcorrplot(index(indices$BSAS)[3:8,])
dev.off()

#

dy <- 2024

# Cohort plot BTS

taf.png("bts.coast_cohort.png")
ggplot(index(indices[["BTS.COAST"]]), aes(x=cohort, y=log(data), group=age)) +
  geom_line(aes(colour=factor(age))) +
  geom_point(colour='white', size=5) +
  geom_text(aes(label=age)) +
  xlab("Cohort") + ylab("log(abundance)") +
  xlim(c(2001, an(dy) - 1)) + ylim(c(0, 10)) +
  theme(legend.position="none")
dev.off()

# Cohort plot BSAS

taf.png("bsas_cohort.png")
ggplot(index(indices[["BSAS"]]), aes(x=cohort, y=log(data), group=age)) +
  geom_line(aes(colour=factor(age))) +
  geom_point(colour='white', size=5) +
  geom_text(aes(label=age)) +
  xlab("Cohort") + ylab("log(abundance)") +
  xlim(c(2004, an(dy) - 1)) +
  theme(legend.position="none")
dev.off()

# }}}

# Catch curve BTS.COAST

taf.png("bts.coast_catchcurve.png")
plotCatchcurve(index(bts.coast.idx)) + 
  ggtitle("Catch Curve: BTS+COAST")
dev.off()

# Catch curve BSAS

taf.png("bsas_catchcurve.png", width = 800)
plotCatchcurve(index(bsas.idx)) + 
  ggtitle("Catch Curve: BSAS")
dev.off()

# }}}
