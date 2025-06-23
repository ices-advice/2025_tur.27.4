## Run analysis with stockassessment version 

## Before:
## After:

library(stockassessment)

# load data (conf, data, par, and fit objects)
load("model/TUR_27.4_model.Rdata")  # 

# Plots
ssbplot(fit)
fbarplot(fit)
recplot(fit)
catchplot(fit)

## Model diagnostics
# One step ahead residuals

# Observation errors
res <- residuals(fit) 
plot(res)

# Process errors
resp <- procres(fit)  
plot(resp)

# Retro
retro <- stockassessment::retro(fit,year=5)
plot(retro)
mohn(retro)

# Leave one out analysis
lo <- leaveout(fit)
plot(lo)

# Obscorrplot
obscorrplot(fit)

# Tables
stockassessment::catchtable(fit)
stockassessment::ssbtable(fit)
stockassessment::fbartable(fit)
stockassessment::rectable(fit)

# Model fit
fitplot(fit, fleets = 1, pch = 20, cex = 2) # Commercial
fitplot(fit, fleets = 2, pch = 20, cex = 2) # BTS.COAST
fitplot(fit, fleets = 3, pch = 20, cex = 2) # BSAS

# Residual diagnostics stockassessment
stockassessment::residplot(fit)
