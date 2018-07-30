#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# Simhyd modelling of the data with gridded rainfall data

# INPUT FILES:
# 20160804_DataIncludingGridded.Rdata
# includes, flow, rainfall (station) maxT and gridRain data

##################
##  ~ Set up ~  ##
##################
# SET WORKING DIRECTORY # #####
#setwd("/home/562/wxv562/MD_Projectdata")
setwd("/project/RDS-FSC-CCH-RW/MDProjectdata")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(hydromad)
library(doParallel)
require(Rcpp)
#####

rcode_dir <-"/project/RDS-FSC-CCH-RW/MDProjectdata/Simhyd" 
source(paste(rcode_dir,"Simhyd.R",sep="/"))

#####
# read in the data
load("Data/ClimCh_project_MD.Rdata")
load("Data/DailyDataIncludingGridded.Rdata")
GridRain <- GridRainAllDataout
rm(flow_rain_maxT_weekly)

nc <- 10 # number of cores
n <- 10 # number of SCE runs
registerDoParallel(cores=nc) 

## 1. Optimisation functions
# SCEOptim  function
SCEfit <- function(mod) {
  fit.Q <- fitBySCE(mod,  objective=~hmadstat("viney")(Q, X),
                    control=list(ncomplex=20))
  s <- summary(fit.Q)
  #rm(fit.Q)
  return(c(do.call(rbind,s[7:10])[,1],coef(fit.Q)))
} 

# fitByOptim function
Ofit <- function(mod,Store) {
  bestFit <- fitByOptim(mod,objective = ~hmadstat("viney")(Q, X),
                        samples = nrow(Store), sampletype = "all.combinations", 
                        initpars = Store[1:nrow(Store),5:9], multistart = T)   
  s <- summary(bestFit)
  return(list(coef=c(do.call(rbind,s[7:10])[,1],coef(bestFit)),
              mod = bestFit))
}

# 2. Calibration function
# Write a function to calibrate each station
Calib.fun <- function(flow,Rain,maxT,station,nr=10,
                      start.t="1970-01-01", 
                      end.t="2010-12-31") {
  # flow is the flow data (as a zoo series)
  # Rain is the rainfall data (as a zoo series)
  # maxT is maximum temperature as a zoo series
  # station is the station name
  # nr is the number of runs
  # start.t start date for calibration period
  # end.t is end date for calibration period
  #rm(output)
  #output <- list()
  indata <- merge(P=Rain,Q=flow,E=maxT,all=T)
  data.cal <- window(indata, start = start.t,end = end.t)
  rm(indata)
  # set some output options
  hydromad.options(trace=TRUE)
  options(warn=1)
  # Define the model
  mod.Q <- hydromad(DATA=data.cal,
                    sma="simhyd_eWater", routing="simhydrouting",
                    COEFF=c(0,400), SQ=c(0.1,5), 
                    K=c(0,1), return_state=TRUE)
  
  # Change hmadstat("rel.bias")
  hydromad.stats("rel.bias" = function(Q, X, ...) {
    ok <- complete.cases(coredata(X), coredata(Q))
    rb <- mean((coredata(X) - coredata(Q))[ok])/mean(coredata(Q)[ok])
    rb <- ifelse(abs(rb)>1,1,rb)
    return(rb)
  })
  # use Viney's objective function(includes Bias), 
  # see http://hydromad.catchment.org/#hydromad.stats
  hydromad.stats("viney" = function(Q, X, ...) {
    hmadstat("r.squared")(Q, X, ...) -
      5*(abs(log(1+hmadstat("rel.bias")(Q,X, ...)))^2.5)})
  
  
  # run SCE 10 times
  Store = foreach(j = 1:nr, .combine=rbind) %dopar%
  {
    # run each of the SCE calibrations  
    run <- as.numeric(SCEfit(mod.Q))
  }
  rm(data.cal)
  
  colnames(Store) <- c("rel.bias","r.squared","r.sq.sqrt","r.sq.log",
                       "DELAY","X_m","impTh","pFrac","INSC","COEFF",
                       "SQ","SMSC","SUB","CRAK","K","etmult")
    # now run fitByOptim
  O.fit <- Ofit(mod.Q,Store)
  
  # Store the coefficients
  # Store <- as.data.frame(rbind(Store,as.numeric(O.fit$coef)))
  Store <- as.data.frame(Store)
  Store$station <- rep(station,nrow(Store))
  #output$Store <- Store
  #rm(Store)
  # Store the model
  #output$mod <- O.fit$mod
  # return output
  return(list(Store=Store,mod=O.fit$mod))
  rm(O.fit); rm(Store)
  }


# 3. Now run over the stations
#for (i in 1:length(Stations)) {
#for (i in 11:12) {
  i <- 13 # testing
  # load(paste(Today,"CalibInputData.Rdata",sep="_"))
  # Create storage frames
  # Run the calibration												
  Output <- Calib.fun(flow = flow_zoo[,i],
                      Rain = zoo(GridRain[GridRain$Station==Stations[i,1],2],
                                 order.by=time(rain_zoo)),
                      maxT = maxT_zoo[,i], 
                      station = Stations[i,1], nr=n)
  save(Output,
       file = paste(Today,paste(Stations[i,1], 
                                "gridSimhydCalibOutput.Rdata", sep=""),sep="_"))
  rm(Output)
#}



