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
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/streamflow_data")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(ggplot2)
require(hydromad)
library(doParallel)
#####
# STATIONS # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")

# read in the data
load("C:/Users/rver4657/ownCloud/Virtual Experiments/ProjectData/20150909_ClimCh_project_MD.Rdata")

cl_in <- 2 # number of clusters
# Set up parallel library
cl <- makeCluster(cl_in)
registerDoParallel(cl)


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
                        samples = 10, sampletype = "all.combinations", 
                        initpars = Store[1:10,5:9], multistart = T)   
  s <- summary(bestFit)
  return(list(coef=c(do.call(rbind,s[7:10])[,1],coef(bestFit)[c(4,1:3,5)])),
         mod = bestFit)
}

# 2. Calibration function
# Write a function to calibrate each station
Calib.fun <- function(flow,Rain,maxT,station,Store) {
  output <- list()
  indata <- merge(P=Rain,Q=flow,E=maxT,all=T)
  data.cal <- window(indata, start = "1970-01-01",end = "1979-12-31")
  # report the mass balance
  output$balance.cal <- sum(indata$P, na.rm=T) - sum(indata$Q, na.rm=T) -
        sum(indata$aET, na.rm=T)
  # set some output options
  hydromad.options(trace=TRUE)
  options(warn=1)
  # Define the model
  mod.Q <- hydromad(DATA=indata,
            sma = "gr4j", routing = "gr4jrouting", 
            x1 = c(100,1200), x2 = c(-30,5), x3 = c(20,500), x4 = c(1.1,2.9), 
            etmult=c(0.05,0.5), return_state=TRUE)
  
  # run SCE 10 times
  # set up the clusters
  clusterExport(cl, c("mod.Q","Store","indata"))
  
  foreach(k=1:10) %dopar% {
    library(hydromad)
    library(zoo)
    # use Viney's objective function(includes Bias), 
    # see http://hydromad.catchment.org/#hydromad.stats
    hydromad.stats("viney" = function(Q, X, ...) {
      hmadstat("r.squared")(Q, X, ...) -
        5*(abs(log(1+hmadstat("rel.bias")(Q,X)))^2.5)})
    
    #    for (j in 1:10) {
    run <- SCEfit(mod.Q)
    Store[k,] <- as.numeric(run)
  }
  # now run fitByOptim
  O.fit <- Ofit(mod.Q,Store)
  
  # Store the coefficients
  Store[11,] <- as.numeric(O.fit$coef)
  Store$station <- station
  output$Store <- Store
  # Store the model
  output$mod <- O.fit$mod
  # return output
  return(output)
}


# 3. Now run over the stations
for (i in seq_along(flow_stns)) {
#i <- 1 # testing
#  load(paste(Today,"CalibInputData.Rdata",sep="_"))
  Store_in <- data.frame(rel.bias = numeric(length=11),r.squared=numeric(length=11),
                                                     r.sq.sqrt=numeric(length=11),r.sq.log=numeric(length=11),
                                                     x1=numeric(length=11),x2=numeric(length=11),x3=numeric(length=11),
                                                     x4=numeric(length=11),etmult=numeric(length=11))
  assign(paste(colnames(flow_stns[i]), "CalibOutput", sep=""), 
         Calib.fun(flow = flow_zoo[,i], Rain = rain_zoo[,i],
                  maxT = maxT_zoo[,i], station = colnames(flow_stns)[i],
                  Store = Store_in))
  save(paste(colnames(flow_stns[i]), "CalibOutput", sep=""),
       paste(Today,paste(colnames(flow_stns[i]), "CalibOutput", sep=""),sep="_"))  
}



