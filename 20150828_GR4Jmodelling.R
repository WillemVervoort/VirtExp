#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# GR4J mdodelling of the data

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
library(xts)
library(zoo)
require(ggplot2)
require(hydromad)
#####
# STATIONS # #####
#flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
#####
# FLOW, RAINFALL AND TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
# Flow data
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""),
         read.csv(paste(flow_stns[,i],"_daily_ts2.csv", sep=""), 
                  col.names=c("Date", "Q")))
}
# fix this up, dates not working
for (i in seq_along(flow_stns)) {
  temp <- get(paste(colnames(flow_stns[i]), "_daily_flow", sep=""))
  year <- substr(as.character(temp$Date),nchar(as.character(temp$Date))-1,nchar(as.character(temp$Date)))
  Dates <- as.Date(paste(substr(as.character(temp$Date),1,nchar(as.character(temp$Date))-2),
                     ifelse(as.numeric(year)>=50,paste("19",year,sep=""),paste("20",year,sep="")),sep=""),
                   "%d/%m/%Y")
  assign(paste(colnames(flow_stns[i]), "_daily_flow.z", sep=""),
         zoo(temp$Q,order.by=Dates))
}

#head(COTT_daily_flow.z)

flow_data <- merge(COTT_daily_flow.z, RUTH_daily_flow.z, 
                   CORA_daily_flow.z, ELIZ_daily_flow.z, COCH_daily_flow.z, 
                   COEN_daily_flow.z, SCOT_daily_flow.z, HELL_daily_flow.z, 
                   NIVE_daily_flow.z, MURR_daily_flow.z, SOUT_daily_flow.z, 
                   YARR_daily_flow.z, DOMB_daily_flow.z, all=T)
colnames(flow_data) <- colnames(flow_stns)
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")
flow_data <- window(flow_data, start = start_date,end = end_date)

## Rainfall stations
closerainfall_stns <- data.frame("COTT"="070316", "RUTH"="069003", "CORA"="069049", "ELIZ"="014149", "COCH"="031083", "COEN"="027005", "SCOT"="023734", "HELL"="091040", "NIVE"="096046", "MURR"="088028", "SOUT"="085238", "YARR"="009538", "DOMB"="009590")
for (i in seq_along(flow_stns)) {
  temp<-read.csv(paste("IDCJAC0009_",closerainfall_stns[,i],"_1800_Data.csv", sep=""))
  temp$Date<-ISOdate(year=temp$Year, month=temp$Month, day=temp$Day)
  temp$Date<-as.Date(temp$Date)
  temp<-subset(temp, Date>=start_date & Date<=end_date, select=c(Date, Rainfall))
  assign(paste(colnames(flow_stns[i]), "temp", sep=""), zoo(temp$Rainfall, order.by=temp$Date))
  print(i)
}
Rain_data<-merge(COTTtemp,RUTHtemp,CORAtemp,ELIZtemp,
            COCHtemp,COENtemp,SCOTtemp,HELLtemp,
            NIVEtemp,MURRtemp,SOUTtemp,YARRtemp,
            DOMBtemp,all=T)
colnames(Rain_data) <- colnames(flow_stns)

## Temperature data
HQmaxT_stns <- data.frame("COTT"="070351", "RUTH"="070351", 
                          "CORA"="068072", "ELIZ"="014015", 
                          "COCH"="034084", "COEN"="027045", 
                          "SCOT"="023373", "HELL"="096003", 
                          "NIVE"="096003", "MURR"="085072", 
                          "SOUT"="085072", "YARR"="009021", 
                          "DOMB"="009518")
for (i in seq_along(flow_stns)) {
  temp <- read.csv(paste(HQmaxT_stns[,i],".csv", sep=""),colClasses=c("character","numeric"))
  temp$Date <- as.Date(temp$Date, format="%d/%m/%Y")
  temp$maxT[temp$maxT==99999.9] <- NA
  temp<-subset(temp, Date>=start_date & Date<=end_date)
  assign(paste(colnames(flow_stns[i]), "temp.maxT", sep=""), zoo(x=temp$maxT, order.by=temp$Date))
  print(i)
}
maxT_data<-merge(COTTtemp.maxT,RUTHtemp.maxT,CORAtemp.maxT,ELIZtemp.maxT,COCHtemp.maxT,COENtemp.maxT,SCOTtemp.maxT,HELLtemp.maxT,NIVEtemp.maxT,MURRtemp.maxT,SOUTtemp.maxT,YARRtemp.maxT,DOMBtemp.maxT)
colnames(maxT_data)<-colnames(flow_stns)

# put in list and object
alldata <- list(flow_data=flow_data,Rain_data=Rain_data,maxT_data=maxT_data)
#save object
save(alldata,file=paste(Today,"CalibInputData.Rdata",sep="_"))

#####



## Set up Hydromad and GR4J
# use Viney's objective function(includes Bias), 
# see http://hydromad.catchment.org/#hydromad.stats
hydromad.stats("viney" = function(Q, X, ...) {
  hmadstat("r.squared")(Q, X, ...) -
    5*(abs(log(1+hmadstat("rel.bias")(Q,X)))^2.5)})

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

# Write a function to calibrate each station
Calib.fun <- function(flow,Rain,maxT,station,Store) {
  output <- list()
  indata <- merge(P=Rain,Q=flow,E=maxT,all=T)
  data.cal <- window(indata, start = "1970-01-01",end = "1979-12-31")
  # report the mass balance
  output$balance.cal <- sum(indata$P, na.rm=T) - sum(indata$Q, na.rm=T) -
        sum(indata$aET, na.rm=T
  # set some output options
  hydromad.options(trace=TRUE)
  options(warn=1)
  # Define the model
  mod.Q <- hydromad(DATA=indata,
            sma = "gr4j", routing = "gr4jrouting", 
            x1 = c(100,1200), x2 = c(-30,5), x3 = c(20,500), x4 = c(1.1,2.9), 
            etmult=c(0.05,0.5), return_state=TRUE)
  # run SCE 10 times
  for (j in 1:10) {
    Store[j,] <- SCEfit(mod.Q)    
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


# split calibration by catchment and by iteration


# can be parallel
load(paste(Today,"CalibInputData.Rdata",sep="_"))
for (i in seq_along(flow_stns)) {
  Store_in <- data.frame(rel.bias = numeric(length=11),r.squared=numeric(length=11),
                                                     r.sq.sqrt=numeric(length=11),r.sq.log=numeric(length=11),
                                                     x1=numeric(length=11),x2=numeric(length=11),x3=numeric(length=11),
                                                     x4=numeric(length=11),etmult=numeric(length=11))
  assign(paste(colnames(flow_stns[i]), "CalibOutput", sep=""), 
         Calibfun(flow = flow_data[,i], Rain = Rain_data[,i],
                  maxT = maxT_data[,i], station = colnames(flow_stns)[i],
                  Store = Store_in))
  save(paste(colnames(flow_stns[i]), "CalibOutput", sep=""),
       paste(Today,paste(colnames(flow_stns[i]), "CalibOutput", sep=""),sep="_"))  
}



