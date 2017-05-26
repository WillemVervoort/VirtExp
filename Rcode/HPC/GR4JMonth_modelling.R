#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# GR4J modelling of the monthly data

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

# read in the monthly data
load("MonthlyDataOut.Rdata")
flow_zoo_m <- dataOut_Month[[1]]
rain_zoo_m <- dataOut_Month[[2]]
maxT_zoo_m <- dataOut_Month[[3]]
flow_rain_maxT_monthly <-dataOut_Month[[4]]
Stations <- read.csv("CatchmentCharact.csv")

nc <- 10 # number of cores
n <- 10 # number of SCE runs
registerDoMC(cores=nc) 

# change hydromad warmup to 12 months
hydromad.options(warmup=12)
##  The current default objective is not suitable for monthly data
hydromad.options(objective=hmadstat("r.squared"))


## 1. Optimisation functions
# SCEOptim  function
SCEfit <- function(mod) {
  fit.Q <- fitBySCE(mod,
                    control=list(ncomplex=20))
  s <- summary(fit.Q)
  #rm(fit.Q)
  return(c(do.call(rbind,s[7:10])[,1],coef(fit.Q)))
} 

# fitByOptim function
Ofit <- function(mod,Store) {
  bestFit <- fitByOptim(mod,
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
                      end.t="1979-12-31") {
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
            sma = "gr4j", routing = "gr4jrouting", 
            x1 = c(20,2000), x2 = c(-50,30), x3 = c(20,1000), x4 = c(0.5,20), 
            etmult=c(0.01,0.5), return_state=TRUE)
  

  # run SCE 10 times
  Store = foreach(j = 1:nr, .combine=rbind) %dopar%
  {
    # run each of the SCE calibrations  
    run <- as.numeric(SCEfit(mod.Q))
  }
  rm(data.cal)
  # now run fitByOptim
  colnames(Store) <- c("rel.bias","r.squared","r.sq.sqrt","r.sq.log",
                       "x2","x3","x4","x1","etmult")
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
for (i in seq_along(Stations[,1])) {
  #i <- 1 # testing
  # load(paste(Today,"CalibInputData.Rdata",sep="_"))
  # Create storage frames
  # Run the calibration												
  Output <- Calib.fun(flow = flow_zoo_m[,i], Rain = rain_zoo_m[,i],
                      maxT = maxT_zoo_m[,i], 
                      station = Stations[i,1], nr=n)
  save(Output,
       file = paste(Today,paste(Stations[i,1], 
                                "GR4JMonthCalibOutput.Rdata", sep=""),sep="_"))
  rm(Output)
}



