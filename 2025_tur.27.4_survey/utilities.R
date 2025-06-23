# utilities.R - DESC
# 2024_sol.27.4_benchmark/boot/utilities.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ggplotFL)

# DATRASraw {{{

DATRASraw <- function(ca, hh, hl, strict=FALSE) {

  d <- list(CA = ca, HH = hh, HL = hl)
  
  for(i in 1:3) d[[i]] <- DATRAS:::renameDATRAS(d[[i]])

  d <- DATRAS:::minus9toNA(d)
  if (is.null(d$CA$StatRec))
    d$CA$StatRec <- d$CA$AreaCode
  d <- DATRAS:::addExtraVariables(d)
  d <- DATRAS:::fixMissingHaulIds(d, strict = strict)

  for(i in 1:3){
    for(k in 1:ncol(d[[i]])){
      if(is.character( d[[i]][,k] ) )
        d[[i]][,k] <- factor(d[[i]][,k])
    }
  }

  class(d) <- "DATRASraw"

  return(d)
}
# }}}

# xvalSAM {{{

xvalSAM <-function(fit, year=NULL , fleet=NULL, age=NULL, ...) {

  data <- fit$data
  nam <- c("year", "fleet", "age")[c(length(year) > 0, length(fleet) > 0,
    length(age) > 0)]

  if((length(year) == 0) & (length(fleet) == 0) & (length(age) == 0)){

    idx <- rep(TRUE, nrow(data$aux))

  } else {

  idx <- !do.call(paste, as.data.frame(data$aux[,nam,drop = FALSE])) %in%
    do.call(paste, as.data.frame(cbind(year=year, fleet=fleet, age=age)))
  }

  idx <- !idx

  data$logobs[idx] <- NA

  idx2 <- which(is.na(data$logobs))

  conf <- fit$conf

  par <- defpar(data,conf)

  thisfit <- sam.fit(data, conf, par, rm.unidentified = TRUE,
    newtonsteps=0, silent=TRUE, ...)

  ret <- as.data.frame(cbind(data$aux[idx2,], obs=fit$data$logobs[idx2],
    pred = thisfit$pl$missing, predSd = thisfit$plsd$missing))

  ret <- ret[complete.cases(ret),]

  attr(ret,"fit") <- thisfit

  return(ret)
}
# }}}

# fixDepth {{{

library(mgcv)

fixDepth <- function(dat) {

  # FIND depth outliers

  hh <- data.table(dat[['HH']])

  # FIT GAM

  dmodel <- gam(log(Depth) ~ s(lon, lat, k=200), data=hh)

  # PREDICT all locations

  hh[, eDepth := mapply(function(x, y)
    exp(predict(dmodel, newdata=data.frame(lon=x, lat=y))), lon, lat)]

  # SELECT eDepth if abs(diff) > 1.5

  hh[, dDepth := abs(Depth / eDepth) > 1.5]

  hh[dDepth == 1 | is.na(dDepth), Depth := round(eDepth)]

  dat[['HH']]$Depth <- hh[, Depth]

  return(dat)
}

# }}}

library(doParallel)

# setNage {{{
setNage <- function(dat, ages) {

  # SPLIT by Year
  dys <- split(dat, dat$Year)

  # CALL fitALK by year

  fdys <- foreach(i=dys, .errorhandling = "stop") %dopar% {
    DATRAS::fitALK(i, minAge=min(ages), maxAge=max(ages), autoChooseK=TRUE,
      useBIC=TRUE, varCof=FALSE, maxK=50)
  }
  
  # PREDICT NaA by year
  pdys <- foreach(i=fdys, .errorhandling = "stop") %dopar% {
    predict(i)
  }

  ndys <- mapply(function(x, y) {
    x$Nage <- y
    return(x) 
  }, x=dys, y=pdys)

  dat <- do.call("c", ndys)

  return(dat)
}
# }}}

# si2FLIndex {{{

si2FLIndex <- function(si, ...) {

  ages <- colnames(si$idx)
  years <- rownames(si$idx)
  dmns <- list(age=ages, year=years)

  # index
  idx <- FLQuant(t(si$idx), dimnames=dmns)

  # lowq, uppq
  lowq <- FLQuant(t(si$lo), dimnames=dmns)
  uppq <- FLQuant(t(si$up), dimnames=dmns)

  # FLIndex(index=FLQuantPoint(mean=idx, lowq=lowq, uppq=uppq))
  FLIndex(index=idx, ...)
} # }}}

# si2FLQuantPoint {{{

si2FLQuantPoint <- function(si, ...) {

  ages <- colnames(si$idx)
  years <- rownames(si$idx)
  dmns <- list(age=ages, year=years)

  # mean
  mean <- FLQuant(unname(t(si$idx)), dimnames=dmns)

  # lowq, uppq
  lowq <- FLQuant(unname(t(si$lo)), dimnames=dmns)
  uppq <- FLQuant(unname(t(si$up)), dimnames=dmns)

  FLQuantPoint(mean=mean, lowq=lowq, uppq=uppq)
} # }}}

# si2FLQuant {{{

si2FLQuant <- function(si, ...) {

  ages <- colnames(si$idx)
  years <- rownames(si$idx)
  dmns <- list(age=ages, year=years)

  return(FLQuant(unname(t(si$idx)), dimnames=dmns))
} # }}}

# biomassFit {{{

biomassFit <- function(fit, dat, index=2) {

  # GET index
  idx <- getFleet(fit, index)

  # GET stock.wt
  wt <- dat$stockMeanWeight[rownames(idx), colnames(idx)]

  logidx <- log(idx)
  logtsidx <- log(rowSums(wt * idx, na.rm=TRUE))

  idx[is.na(idx)] <- 0

  dlogtsbidx <- wt * idx / rowSums(wt * idx, na.rm=TRUE)

  cov <- fit$rep$obsCov[[index]]

  sdlogtsbidx <- sqrt(sapply(1:nrow(dlogtsbidx),
    function(i) dlogtsbidx[i, , drop=FALSE] %*% cov %*% 
      t(dlogtsbidx[i, , drop=FALSE])))
  
  N <- ntable(fit)[rownames(idx), colnames(idx)]
  M <- dat$natMor[rownames(idx), colnames(idx)]
  F <- faytable(fit)[rownames(idx), colnames(idx)]
  Q <- t(replicate(nrow(idx),
    exp(fit$pl$logFpar[fit$conf$keyLogFpar[index, ] + 1])))

  tau <- dat$sampleTimes[index]

  pred <- rowSums(Q * N * exp(-tau * (F + M)) * wt)

  year <- as.integer(names(logtsidx))

  return(data.frame(year=year, obs=unname(exp(logtsidx)), pred=unname(pred),
    low=unname(exp(log(pred) - 2 * sdlogtsbidx)),
    upp=unname(exp(log(pred) + 2 * sdlogtsbidx))))
}
# }}}

# plot_biomassFit {{{
plot_biomassFit <- function(fit, dat, index=2) {

  dat <- biomassFit(fit, dat, index=index)

  ggplot(dat, aes(x=year)) +
    geom_point(aes(y=obs)) +
    geom_line(aes(y=pred)) +
    geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)
 #   geom_errorbar(aes(ymin=low, ymax=upp), width=1)
}
# }}}

# z-scores {{{

zscore <- function(x) {
  return((x %-% yearMeans(x)) %/% sqrt(yearVars(x)))
}
# }}}

# spatialPreds {{{

spatialPreds <- function(x, grid) {

  # x$gPreds2: ages, years
  # grid: years

  pos <- data.table(lon=grid[[1]]$lon, lat=grid[[1]]$lat)

  res <- rbindlist(lapply(setNames(x$gPreds2, nm=x$dataAges),
    function(a) {
      browser()
    cbind(pos, rbindlist(lapply(a, function(i)
      data.table(pred=c(i))), idcol='year'))
    }), idcol='age')

  return(res)
}
# }}}

# gs {{{
gs <- function(filename=NULL, device='png') {

  if(is.null(filename))
    filename <- paste0('explore/plot-', gsub(' ', '_', date()), ',', device)

  ggsave(filename, device=device)
}
# }}}

# plotCatchcurve {{{
plotCatchcurve <- function(x) {

  dat <- as.data.frame(x, cohort=TRUE)
  yrs <- unique(dat$year)

  ggplot(dat, aes(x=ISOdate(year, 1, 1), y=log(data),
    group=factor(age), color=factor(age))) +
    geom_line(aes(group=factor(cohort)), color="black", alpha=0.4) +
    geom_point(colour='white', size=5) + 
    geom_text(aes(label=age), fontface = "bold") +
    xlab("Cohort") + ylab("") +
    theme(legend.position="none")
}
# }}}


######## From nsea_functions.r

########--------------------------+++++++++-----------------------------########
# NAME: plotinternal
# DOES: have our own plotinternal function that shows more info

panel.ci1 <- function(x, y, interval='prediction',...) {	
  
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  rsq <-  sprintf("%4.3f",round(summary(fit)$r.squared,3))
  newX    <- data.frame(x=seq(min(x)-5,max(x)+5,0.01))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, ...)
  panel.lmline(x, y, ..., identifier = "lmline")
  panel.xyplot(newX$x, fitPred[,2], type="a", lty=2, ...)
  panel.xyplot(newX$x, fitPred[,3], type="a", lty=2, ...)
  grid::grid.text(label = rsq,x = unit(0, "npc") + unit(0.25,"lines"), y = unit(1, "npc") - unit(0.25,"lines"),just="left",gp=gpar(cex=0.8)) 
  
}

plotinternal <- function(x, marklast=T ,... ) 
{ 
  
  skip <- matrix(1,nrow=dims(x@index)$age-1, ncol=dims(x@index)$age-1) 
  xydf <- NULL 
  for (age in dims(x@index)$min:(dims(x@index)$max-1) ) 
  { 
    for (inc in 1:(dims(x@index)$max-age)) 
    { 
      years <- dims(x@index)$minyear:(dims(x@index)$maxyear-inc) 
      xd <- as.numeric(x@index[as.character(age),as.character(years),]) 
      yd <- as.numeric(x@index[as.character(age+inc),as.character(years+inc),]) 
      d <- paste("age",age,"vs",age+inc) 
      xydf <- rbind(xydf,cbind(as.data.frame(cbind(xd,yd)),d)) 
      skip[dims(x@index)$max-dims(x@index)$min+1-inc, age-dims(x@index)$min + 1] <-0 
    } 
  } 
  xydf <- xydf[xydf$xd != 0 & xydf$yd != 0 & !is.na(xydf$xd) & !is.na(xydf$yd),] 
  
  if (marklast){
    print(xyplot(yd~xd|d,outer=F, col="black", data=xydf, panel=
                   function(x,y,marklast,...) {
                     panel.xyplot(x,y, ...)
                     panel.xyplot(x[length(x)], y[length(y)], pch=16, ...)
                     panel.ci1(x,y,...)}, 
                 scales=list(log="e",relation="sliced",draw=FALSE), layout=c(dims(x@index)$age-1, 
                                                                             dims(x@index)$age-1), xlab="log index", ylab="log index", skip=as.numeric(skip),  
                 main=x@name))
  } else {
    print(xyplot(yd~xd|d,outer=F, col="black", data=xydf, panel=
                   function(x,y,marklast,...) {
                     panel.xyplot(x,y, ...)
                     panel.ci(x,y,...)
                   }, 
                 scales=list(log="e",relation="sliced",draw=FALSE), layout=c(dims(x@index)$age-1, 
                                                                             dims(x@index)$age-1), xlab="log index", ylab="log index", skip=as.numeric(skip),  
                 main=x@name))
  }
  
} # }}} 

# NAME: plotinternal
# DOES: have our own plot internal consistency plot function that shows more info

panel.ci2 <- function(x, y, interval='prediction',...) {	
  
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  rsq <-  sprintf("%4.3f",round(summary(fit)$r.squared,3))
  newX    <- data.frame(x=seq(min(x)-5,max(x)+5,0.01))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, ...)
  panel.lmline(x, y, ..., identifier = "lmline")
  panel.xyplot(newX$x, fitPred[,2], type="a", col="black", lty=2, ...)
  panel.xyplot(newX$x, fitPred[,3], type="a", col="black", lty=2, ...)
  
}

# internal consistency  {{{ 
plotInternalConsistency <-  function(idx,log.scales=TRUE, 
                                     cols=adjustcolor(c("white", "yellow", "red"), 0.2),use.rsq=TRUE,mark.significant=FALSE,mark.last=F,...) 
{ 
  
  
  # Define colour function 
  if(!length(cols)>0) stop("Colour definitions do not contain sufficient number of colours (at least one required)") 
  if(length(cols)==1) cols <- rep(cols,2) 
  colFn <- colorRamp(colors=cols) 
  
  
  #Number of ages 
  ages <- dimnames(idx@index)[[1]] 
  
  
  #Convert to Cohorts, reshape into appropriate format for splom 
  flc <-  if(log.scales) {log10(idx@index)}  
  else {idx@index} 
  flc  <- as.data.frame(FLCohort(flc)) 
  flc.wide <-  reshape(flc,direction="wide",timevar=names(flc)[1],idvar=names(flc)[2:6]) 
  names(flc.wide) <-  gsub("data.","",names(flc.wide)) 
  
  #Default plot settings 
  plot.args <- list(~flc.wide[ages],data=flc.wide, pscales=0,varname.font=2,varname.cex=1.5, 
                    xlab = if(log.scales) {expression(paste(Log[10]," (Index Value)"))}  
                    else {"Index Value"}, 
                    ylab = if(log.scales) {expression(paste(Log[10]," (Index Value)"))} 
                    else { "Index Value"}, 
                    sub=list(if(use.rsq) {expression(paste("Lower right panels show the Coefficient of Determination (",italic(r^2),")"))} 
                             else { expression(paste("Lower right panels show the Coefficient of Correlation (",italic(r),")"))},cex=0.7), 
                    upper.panel=function(x,y,...) 
                    { 
                      # Filter out NAs 
                      both.points  <-  is.finite(x) & is.finite(y) 
                      x.filtered <-  x[both.points] 
                      y.filtered <-  y[both.points] 
                      # Only plot lmline if there is more than one point - colour panel according to rsq. 
                      if(length(x.filtered)>2) 
                      { 
                        r <-  cor(y.filtered,x.filtered)     
                        if(use.rsq) { 
                          panel.colour <- r^2           #Colour & number panel based on the coefficient of determination (r^2) 
                        } else { 
                          panel.colour <- 0.5*r+0.5     #Colour & number panel based on the correlation coefficient (r) 
                        } 
                        if(is.numeric(mark.significant) | identical(TRUE,mark.significant) ) { 
                          lm.model <- lm(y.filtered ~ x.filtered) 
                          p.value  <- summary(lm.model)$coefficients["x.filtered",4]/2    #Halve the p-value, as we are doing a one sided test, not a two 
                          slope    <- summary(lm.model)$coefficients["x.filtered",1] 
                          signif.level <- 0.05 
                          if(is.numeric(mark.significant)) signif.level <- mark.significant  
                          if(p.value < signif.level & slope >0) {  #If marking significance, only fill panel and draw line when its significant 
                            number.format <- "%4.3f*"      #If its a significant correlation, mark with a * 
                            panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2) 
                            panel.lmline(x.filtered,y.filtered,lwd=2) 
                          }
                          if (mark.last) panel.xyplot(x=x.filtered[length(x.filtered)], y=y.filtered[length(y.filtered)], pch=15) 
                          
                        } else {  #If not marking significance, always fill panel and draw best fit line 
                          
                          panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2) 
                          panel.ci2(x.filtered,y.filtered,lwd=2) 
                          if (mark.last) panel.xyplot(x=x.filtered[length(x.filtered)], y=y.filtered[length(y.filtered)], pch=15,...) 
                        } 
                      } 
                      panel.splom(x.filtered,y.filtered,col="black",...) 
                    }, 
                    lower.panel=function(x, y, ...) 
                    { 
                      #Filter out NAs 
                      both.points  <-  is.finite(x) & is.finite(y) 
                      x.filtered <-  x[both.points] 
                      y.filtered <-  y[both.points] 
                      
                      #Calculate r squared - but only if there is enough data to do so 
                      if(length(x.filtered)>2) 
                      { 
                        r <-  cor(y.filtered,x.filtered) 
                        if(use.rsq) { 
                          panel.colour <- r^2           #Colour & number panel based on the coefficient of determination (r^2) 
                          panel.number <- round(r^2,3) 
                        } else { 
                          panel.colour <- 0.5*r+0.5  #Colour & number panel based on the correlation coefficient (r) 
                          panel.number <- round(r,3) 
                        } 
                        number.format <- "%4.3f" 
                        if(is.numeric(mark.significant) | identical(TRUE,mark.significant) ) { 
                          lm.model <- lm(y.filtered ~ x.filtered) 
                          p.value  <- summary(lm.model)$coefficients["x.filtered",4]/2 
                          slope    <- summary(lm.model)$coefficients["x.filtered",1] 
                          signif.level <- 0.05 
                          if(is.numeric(mark.significant)) signif.level <- mark.significant  
                          if(p.value < signif.level & slope > 0) {  #If marking significance, only fill panel when its significant & positive 
                            number.format <- "%4.3f*"      #If its a significant correlation, mark with a * 
                            panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2) 
                          }                 
                        } else {  #If not marking significance, always fill panel  
                          panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2) 
                        } 
                        grid::grid.text(label =sprintf(number.format,panel.number),x = unit(0.5, "npc"), 
                                        y = unit(0.5,"npc"),just="center")}}) 
  
  
  #Passed settings 
  passed.args   <- list(...) 
  plot.args[names(passed.args)] <- passed.args 
  
  #Do plot 
  p <- do.call(splom,plot.args) 
  print(p) 
  return(p) 
}   # }}} 

# fitTable, predTable {{{

library(surveyIndex)
library(data.table)

#' fitTable: year, country, lat, lon, age, obs, resid
#' @param fit
#' @param survey

fitTable <- function(fit, survey) {
  
  # GET dimnames
  dmns <- dimnames(fit$idx)
  years <- setNames(seq(dmns[[1]]), dmns[[1]])
  ages <- setNames(seq(dmns[[2]]), dmns[[2]])
  
  res <- rbindlist(lapply(ages, function(a)
    cbind(data.table(survey[['HH']])[, .(Year, ShootLat, ShootLong, Country)],
          data.table(obs=fit$allobs[[a]], resid=fit$residuals[[a]]))), idcol="age")
  
  # SET names, key and column order
  setnames(res, c("age", "year", "lat", "lon", "country", "obs", "resid"))
  setkey(res, year, age, country)
  setcolorder(res, c('year', 'country', 'lat', 'lon', 'age'))
  
  return(res[])
}

# predTable: year, lat, lon, depth, age, pred, cv

predTable <- function(fit, grid) {
  
  # GET dimnames
  dmns <- dimnames(fit$idx)
  years <- setNames(seq(dmns[[1]]), dmns[[1]])
  ages <- setNames(seq(dmns[[2]]), dmns[[2]])
  
  # DOUBLE loop over ages and years
  res <- rbindlist(lapply(ages, function(a)
    rbindlist(lapply(years, function(y)
      cbind(grid[, c('lat', 'lon', 'Depth')], data.frame(
        pred=fit$gPreds2[[a]][[y]],
        sdlog=fit$gPreds2.CV[[a]][[y]],
        resid=fit$residuals[[a]][[y]]))), idcol="year")), idcol="age")
  
  # SET names, key and column order
  setnames(res, "Depth", "depth")
  setkey(res, year, age)
  setcolorder(res, c('year', 'lat', 'lon', 'age'))
  
  #
  foo <- function(x) {
    i <- order(x)
    ys <- sort(exp(x))
    p <- ys/sum(ys)
    x[i] <- cumsum(p)
    x
  }
  
  # NORMALIZE by age & year
  res[, norma := log(pred) / max(log(pred)), by=.(age)]
  res[, normy := log(pred) / max(log(pred)), by=.(year)]
  res[, normaf := foo(pred), by=.(age)]
  res[, normyf := foo(pred), by=.(year)]
  
  # SET column classes
  res[, age := as.numeric(age)]
  res[, year := as.numeric(year)]
  
  return(res[])
}
# }}}

