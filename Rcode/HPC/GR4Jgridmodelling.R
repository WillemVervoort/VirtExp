#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# GR4J modelling with gridded rainfall data

# INPUT FILES:
#"20160804_ClimCh_project_MD.Rdata"
#"GriddedRainfallData.Rdata"
# includes flow rain maxT and gridded rainfall data

##################
##  ~ Set up ~  ##
##################
#setwd("/home/562/wxv562/MD_Projectdata")
setwd("/project/RDS-FSC-CCH-RW/MDProjectdata")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(hydromad)
library(doParallel)
#####

#####
# STATIONS # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")

# read in the data
load("Data/ClimCh_project_MD.Rdata")
load("Data/DailyDataIncludingGridded.Rdata")
GridRain <- GridRainAllDataout
rm(flow_rain_maxT_weekly)
rm(CC)

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
            sma = "gr4j", routing = "gr4jrouting", 
            x1 = c(20,4000), x2 = c(-50,30), x3 = c(20,1000), x4 = c(0.5,20), 
            etmult=c(0.01,0.5), return_state=TRUE)
  
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
for (i in seq_along(flow_stns)) {
  #i <- 1 # testing
  # load(paste(Today,"CalibInputData.Rdata",sep="_"))
  # Create storage frames
  # Run the calibration												
  Output <- Calib.fun(flow = flow_zoo[,i],
                      Rain = zoo(GridRain[GridRain$Station==Stations[i,1],2],
                                 order.by=time(rain_zoo)),
                      maxT = maxT_zoo[,i], 
                      station = flow_stns[i], nr=n)
  save(Output,
       file = paste(Today,paste(colnames(flow_stns[i]), 
                                "GR4JGridCalibOutput.Rdata", sep=""),sep="_"))
  rm(Output)
}



