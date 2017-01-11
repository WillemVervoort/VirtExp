#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# GR4J modelling of the data

# INPUT FILES:
# - Flow data: […]_daily_ts2.csv […] = flow station number
# - Rainfall data: IDCJAC0009_[…]_1800_Data.csv […] = rainfall station number
# - MaxT data: […].csv […] = HQmaxT station number
# - Flow station locations: Flow_station_locations.csv

##################
##  ~ Set up ~  ##
##################
# SET WORKING DIRECTORY # #####
setwd("/home/562/wxv562/MD_Projectdata")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(ggplot2)
require(hydromad)
# doMC only runs under Linux
library(doMC)
require(foreach)
#####
# STATIONS # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")

# read in the data
load("20150925_ClimCh_project_MD.Rdata")

nc <- 10 # number of cores
n <- 10 # number of SCE runs
registerDoMC(cores=nc) 

## 1. Optimisation functions
# SCEOptim  function
SCEfit <- function(mod) {
    fit.Q <- fitBySCE(mod,  objective=~hmadstat("viney")(Q, X),
                         control=list(ncomplex=20))
    s <- summary(fit.Q)
    return(c(do.call(rbind,s[7:10])[,1],coef(fit.Q)[c(4,1:3,5)]))
  } 

# fitByOptim function
Ofit <- function(mod,Store) {
  bestFit <- fitByOptim(mod,objective = ~hmadstat("viney")(Q, X),
                        samples = nrow(Store), sampletype = "all.combinations", 
                        initpars = Store[1:nrow(Store),5:9], multistart = T)   
  s <- summary(bestFit)
  return(list(coef=c(do.call(rbind,s[7:10])[,1],coef(bestFit)[c(4,1:3,5)]),
				mod = bestFit))
}

# 2. Calibration function
# Write a function to calibrate each station
Calib.fun <- function(flow,Rain,maxT,station,nr=10,
                      start.t="1970-01-01", 
                      end.t="1979-12-31") {
  # flow is the flow data (as a zoo series)
  # Rain is the rainfall data (as a zoo series)
  # maxT is maximum temperature as a zoo series
  # station is the station name
  # nr is the number of runs
  # start.t start date for calibration period
  # end.t is end date for calibration period
  output <- list()
  indata <- merge(P=Rain,Q=flow,E=maxT,all=T)
  data.cal <- window(indata, start = start.t,end = end.t)
  rm(indata)
  # set some output options
  hydromad.options(trace=TRUE)
  options(warn=1)
  # Define the model
  mod.Q <- hydromad(DATA=data.cal,
            sma = "gr4j", routing = "gr4jrouting", 
            x1 = c(100,1200), x2 = c(-30,5), x3 = c(20,500), x4 = c(1.1,2.9), 
            etmult=c(0.05,0.5), return_state=TRUE)
  
  # use Viney's objective function(includes Bias), 
  # see http://hydromad.catchment.org/#hydromad.stats
  hydromad.stats("viney" = function(Q, X, ...) {
    hmadstat("r.squared")(Q, X, ...) -
      5*(abs(log(1+hmadstat("rel.bias")(Q,X)))^2.5)})
  
  
  # run SCE 10 times
  Store = foreach(j = 1:nr, .combine=rbind) %dopar%
  {
  # run each of the SCE calibrations  
    run <- as.numeric(SCEfit(mod.Q))
  }
  # now run fitByOptim
  O.fit <- Ofit(mod.Q,Store)
  
  # Store the coefficients
  Store <- as.data.frame(rbind(Store,as.numeric(O.fit$coef)))
  Store$station <- rep(station,nrow(Store))
  output$Store <- Store
  rm(Store)
  # Store the model
  output$mod <- O.fit$mod
  rm(c(O.fit,data.cal))
  # return output
  return(output)
}


# 3. Now run over the stations
for (i in seq_along(flow_stns)) {
#i <- 1 # testing
 # load(paste(Today,"CalibInputData.Rdata",sep="_"))
  # Create storage frames
	# Run the calibration												
  Output <- Calib.fun(flow = flow_zoo[,i], Rain = rain_zoo[,i],
                      maxT = maxT_zoo[,i], 
                      station = colnames(flow_stns)[i], nr=n)
  save(Output,
       file = paste(Today,paste(colnames(flow_stns[i]), 
                                "CalibOutput.Rdata", sep=""),sep="_"))
 rm(Output)  
}



