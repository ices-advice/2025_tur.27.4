## Prepare plots for report
# report.R - DESC
# 2025_tur.27.4_benchmark/report.R

# Copyright (c) WUR, 2025.
# Authors: Justin TIANO (WMR) <justin.tiano@wur.nl>
#          Lennert van de Pol (WMR) <lennert.vandepol@wur.nl>

library(icesTAF)

mkdir("report")

load("data/input.RData")

# Set up paths to directories 
run       <- "Final_"
assYear   <- 2025
codePath  <- paste("boot/initial/data/source/",sep="")
dataPath  <- paste("boot/initial/data/Lowestoft files/",sep="")
figPath   <- paste("report/")
maxA      <- 10
startyr   <- range(stock)[["minyear"]]## Read index data

sens      <- "WGNSSK 2025"

# Load model objects  
load("model/TUR_27.4_model.Rdata")

# Get Figures in png format for presentation in report folder
library(RColorBrewer)
source("boot/initial/data/source/03b_runDiagnostics_for_presentation.r")          

## To load "stock" and Unit input objects
source("boot/initial/data/source/03a_setupStockIndices.r")

# Choose folder in report
figPath   <- paste("report/")   

# Plot: Stock Weights at Age used in model (quarter 2)

cols <- brewer.pal(maxA,"Spectral")
stock.wt2 <- exp(fit$pl$logSW)

tiff(filename = paste0(figPath,"Stock_weights_modeled_1981.tif"),units = "cm", width = 20,height = 20,
     pointsize = 12,res = 150)
plot(x=1981:(assYear+9), y=stock.wt2[,1], ylim=c(0,9.1), xlab="Year", cex.lab=1.2,ylab="Weights (kg)", main="Stock weight@age", lwd=2, col="white" )
for (aa in 1:8){
  points(x=1981:(assYear+9),y=as.numeric(stock.wt2[,aa]), pch= if (aa<8) as.character(aa) else "+", col=cols[aa])
  lines(x=1981:(assYear+9),y=as.numeric(stock.wt2[,aa]), pch= if (aa<8) as.character(aa) else "+", col=cols[aa])
  abline(v = assYear-1)
}
dev.off()

landings.wt(stock)[landings.wt(stock)==0] <- NA

# Plot: Landings Weights at Age used in model

catch.wt2 <- exp(fit$pl$logCW)
catch.wt2 <- catch.wt2[, , 1]

tiff(filename = paste0(figPath,"Catch_weights_modeled_1981.tif"),units = "cm", width = 20,height = 20,
     pointsize = 12,res = 150)
plot(x=1981:(assYear+9), y=catch.wt2[,1], ylim=c(0,9.1), xlab="Year", cex.lab=1.2,ylab="Weights (kg)", main="catch weight@age", lwd=2, col="white" )
for (aa in 1:8){
  points(x=1981:(assYear+9),y=as.numeric(catch.wt2[,aa]), pch= if (aa<8) as.character(aa) else "+", col=cols[aa])
  lines(x=1981:(assYear+9),y=as.numeric(catch.wt2[,aa]), pch= if (aa<8) as.character(aa) else "+", col=cols[aa])
  abline(v = assYear-1)
}
dev.off()


# Discards
## Load data

disc           <- read.table(paste0(dataPath,"diton.txt")) # Discards
disc           <- disc[-1,]
colnames(disc) <- c("year","discards")
str(disc)
disc$discards  <- as.numeric(as.character(disc$discards))
disc$Year      <- as.numeric(as.character(disc$year))

# Plot: Landings and discards (manually added discards, which are in a diton file in lowestoft files and come from intercatch)

tiff(filename = paste0(figPath,"Landings_and_discards.tif"),units = "cm", width = 20,height = 16,
     pointsize = 12,res = 150)
plot(x=startyr:(assYear-1), y=landings(stock)/1000, xlim=c(1975,(assYear-1)), ylim=c(0,1.1*max(landings(stock)))/1000, type="l", xlab="Year",lty=1, ylab= "Landings ('000 t)",main="Total landings and discards", las=1, lwd=2, col="blue")
lines(x=disc$Year[1]:(assYear-1), y=disc$discards/1000, col="red", lwd=2)
grid()
dev.off()

# Landings and Discards as a barchart

Data <- as.data.frame(landings(stock))
Data <- Data[,c(2,7)]
Data <- merge(Data,disc,by="year", all.x=T)
colnames(Data)[2] <- "landings"
Data$discards <- Data$discards
Data$Year <- NULL

library(reshape2);library(ggplot2)
Data <- melt(Data, id.vars = "year")
Data$cut <- factor(Data$variable, levels=c("discards", "landings"))

tiff(filename = paste0(figPath,"Landings_and_discards_barchart.tif"),units = "cm", width = 24,height = 16,
     pointsize = 12,res = 150)

ggplot(data=Data[Data$year > 1980,], aes(x=year,y=value,fill=cut))+
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values=c("#f03b20","#9BCD9B")) +
  scale_y_continuous(limits=c(0,8000),breaks=seq(0,8000,1000),expression("Catch in tonnes"),expand = c(0,0)) +
  scale_x_continuous(limits=c(1980,assYear),breaks=seq(1981,assYear,3),expand = c(0,0)) +
  theme(strip.text        = element_text(face="bold",size=12),
        strip.background  = element_rect(fill="white",colour="black",size=0.3),
        panel.grid.major  = element_line(colour="white"),
        panel.grid.minor  = element_line(colour="white"),
        panel.background  = element_rect(fill="white"),
        panel.border      = element_rect(colour="black", fill=NA, size=0.5),
        axis.text.x       = element_text(colour="black", size=12),
        axis.text.y       = element_text(colour="black", size=12),
        axis.title.y      = element_text(angle=90, size=14),
        axis.title.x      = element_text(size=14),
        legend.position   = " ",
        legend.text       = element_text(colour="black", size=12 ),
        legend.title      = element_text(face="bold",colour="black", size=14 ),
        legend.key        = element_rect(fill = "white"),
        legend.key.size   = unit(1, "cm"),
        panel.spacing.x   = unit(1.5, "lines"),
        panel.spacing.y   = unit(0.9, "lines"),
        plot.margin       = unit(c(0.4,1.1,0.1,1.1), "cm"))
dev.off()

# Bubble plot of Landings at age

bsize <- 0.08 
plc <-  (landings.n(stock))[,ac(1975: (assYear-1))]
# bubbles(age~year, data=resL, col=c("black","black"), bub.scale=10, pch=c(21,21), fill=resL>0, xlim=c(1957,(assYear-1)), ylim=c(0,(maxA+1)), ylab= "Age", main="Landings residuals")
nmages <-  length(dimnames(plc)[[1]])
ylims <- 0.5*nmages+0.25
tiff(filename = paste0(figPath,"Landings_at_age.tif"),units = "cm", width = 20,height = 16,
     pointsize = 12,res = 150)
plot(NA,NA,main="Landings at age", xlab="Year", ylab="Age",xlim=c(as.numeric(min(dimnames(plc)[[2]])),as.numeric(max(dimnames(plc)[[2]]))), yaxt="n", ylim=c(0,ylims))
axis(2,at=seq(0.5,0.5*nmages,by=0.5),labels=as.numeric(dimnames(plc)[[1]]))
for (i in as.numeric(dimnames(plc)[[1]])) {
  radius <- as.numeric(sqrt(abs(plc[i,]) / pi)) *bsize
  points(dimnames(plc)[[2]], rep(0.5*i,ncol(plc)),cex=radius*2, pch=21, col=c("black", "blue")[1+as.numeric(plc[i,]>0)], bg=alpha(c("black", "blue")[1+as.numeric(plc[i,]>0)],0.5))
}
text(2005,0.1,paste("min = ",round(min(c(plc)[!is.infinite(c(plc))],na.rm=T),2),"; max = ",round(max(c(plc)[!is.infinite(c(plc))],na.rm=T),2) ,sep=""), cex=1, pos=4)
dev.off()

# Raw Data
## Read and process assessment input data

## Read stock data
stock               <- readFLStock(paste(dataPath,"index.txt", sep=""))
units(stock)[1:17]  <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))

## Plot raw weights stock and landings

## Plot raw stock weights
maxA  <- 10
cols <- brewer.pal(maxA,"Spectral")
stock.wt(stock)[stock.wt(stock)==0] <- NA

tiff(filename = paste0(figPath,"Stock_weights_1975_raw.tif"),units = "cm", width = 20,height = 20,
     pointsize = 12,res = 150)
plot(x=1975:(assYear-1), y=stock.wt(stock)[1,], ylim=c(0,9.1), xlab="Year", cex.lab=1.2,ylab="Weights (kg)", main="Stock weight@age", lwd=2, col="white" )
for (aa in 1:maxA){
  points(x=1975:(assYear-1),y=as.numeric(stock.wt(stock)[aa,]), pch= if (aa<10) as.character(aa) else "+", col=cols[aa])
  #  lines(x=1975:(assYear-1),y=as.numeric(stock.wt(stock)[aa,]), pch= if (aa<10) as.character(aa) else "+", col=cols[aa])
}
dev.off()

## Plot raw landings weights

landings.wt(stock)[landings.wt(stock)==0] <- NA

##----landing---
tiff(filename = paste0(figPath,"landing_weights_raw.tif"),units = "cm", width = 20,height = 20,
     pointsize = 12,res = 150)
plot(x=1975:(assYear-1), y=landings.wt(stock)[1,], ylim=c(0,9.1), xlab="Year", cex.lab=1.2,ylab="Weights (kg)", main="Landing weight@age", lwd=2, col="white" )
for (aa in 1:maxA){
  points(x=1975:(assYear-1),y=as.numeric(landings.wt(stock)[aa,]), pch= if (aa<10) as.character(aa) else "+", col=cols[aa])
}
dev.off()

## Plots modelled weights at age in 

# Stock weights
png(file="report/GMRF_west.png",width=25,height=20,units="cm",res=150)
  matplot(fit$data$stockMeanWeight, main = "Gaussian Markov Random Field derived stock weights", ylab = "grams", xlab = "years")
  matplot(exp(fit$pl$logSW), type="l", add=TRUE)
  #matplot(fit$pl$logSW-2*fit$plsd$logSW, type="l", add=TRUE, lty="dotted") #SD
  #matplot(fit$pl$logSW+2*fit$plsd$logSW, type="l", add=TRUE, lty="dotted") #SD
dev.off()

# Catch weights  
png(file="report/GMRF_weca.png",width=25,height=20,units="cm",res=150)
  matplot(fit$data$catchMeanWeight[, , "Fleet w.o. effort 1"], main = "Gaussian Markov Random Field derived catch weights", ylab = "grams", xlab = "years")
  matplot(exp(fit$pl$logCW[, , 1]), type="l", add=TRUE)
dev.off()

## One-step ahead residual plots

# Observation errors
res <- residuals(fit) 
# Process residuals
resp <- procres(fit)  

taf.png("report/obs.errors") # 
plot(res)
dev.off()

taf.png("report/process.errors") # 
plot(resp)
dev.off()

# Model fit (use if residual.diagnostics(TUR.sam doesn't work))
taf.png("report/catches.fit") # 
fitplot(fit, fleets = 1, pch = 20, cex = 2) # Commercial
dev.off()

taf.png("report/BTS.COAST.fit") # 
fitplot(fit, fleets = 2, pch = 20, cex = 2) # BTS+COAST
dev.off()

taf.png("report/BSAS.fit") # 
fitplot(fit, fleets = 3, pch = 20, cex = 2) # BSAS
dev.off()

# Survey indices
load("boot/initial/data/surveys/indices.rda")

bts.coast <- as.data.frame(index(indices$BTS.COAST))
size <- log(bts.coast$data + 1) * 4

p1 <- ggplot(data = bts.coast, aes(x = year, y = as.factor(age))) +
  geom_point(shape = 21, color = "black", fill = "black", alpha = 0.6, size = size) +
  scale_size_continuous(range = c(1, 20), name = "Index Value") +
  labs(title = "BTS+COAST", x = "Year", y = "Age") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("report/BTS.COAST.bubbleplot.png", plot = p1, width = 15, height = 8, dpi = 200)

bsas <- as.data.frame(index(indices$BSAS))
size <- log(bsas$data + 1) * 3

p2 <- ggplot(data = bsas, aes(x = year, y = as.factor(age))) +
  geom_point(shape = 21, color = "black", fill = "black", alpha = 0.6, size = size) +
  scale_size_continuous(range = c(1, 20), name = "Index Value") +
  labs(title = "BSAS", x = "Year", y = "Age") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("report/BSAS.bubbleplot.png", plot = p2, width = 10, height = 8, dpi = 200)
