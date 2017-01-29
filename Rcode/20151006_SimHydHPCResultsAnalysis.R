#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# This script is to analyse the results of the HPC experiment

setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/HPC/MD_ProjectData")

require(zoo)
require(xts)
require(hydromad)
require(mgcv)
require(Kendall)
Today <- format(Sys.Date(),"%Y%m%d")

load("20150925_ClimCh_project_MD.Rdata")
source("C:/Users/rver4657/ownCloud/Virtual Experiments/HPC/MD_ProjectRCode/20151104_Simhyd.r")


start.date <- as.Date("1981-01-01")
end.date <- as.Date("2010-12-31")
# Flow stations
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", 
                        "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", 
                        "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497",
                        "MURR"="405205", "SOUT"="225020A", "YARR"="614044", 
                        "DOMB"="607155")

Results <- data.frame(station=names(flow_stns),trend=numeric(length=13),
                      p_trend=numeric(length=13),r_sq=numeric(length=13),
                      tau=numeric(length=13),p_tau=numeric(length=13),
                      Mod.r.sq=numeric(length=13), Mod.bias=numeric(length=13))

# read in the modelling results
for (i in seq_along(flow_stns)) {
  # first test one

#  i <- 1
  # load the rainfall, ET and flow data
  pred.data <- window(merge(flow_zoo[,i], rain_zoo[,i], maxT_zoo[,i]),
                      start=start.date, end=end.date)
  colnames(pred.data) <- c("Q","P","E")
  
  # load the relevant output
  load(paste("20151103_",names(flow_stns)[i],"CalibOutput.Rdata",sep=""))
  # extract the model and update with the parameters
  Mod <- Output$mod
  # check for minimum obj function
  if (mean(Output$Store[1:10,1])<Output$Store[11,1]) {
    Mod <- update(Mod, INSC=mean(Output$Store[1:10,5]),COEFF=mean(Output$Store[1:10,6]),
                      SQ=mean(Output$Store[1:10,7]),SMSC=mean(Output$Store[1:10,8]),
                      SUB=mean(Output$Store[1:10,9]),CRAK=mean(Output$Store[1:10,10]),
                      K=mean(Output$Store[1:10,11]),SMSC=mean(Output$Store[1:10,12]),
                    etmult=mean(Output$Store[1:10,13]),
                  return_state=F)
    
   
             
  } else {
    Mod <- update(Mod, INSC=mean(Output$Store[11,5]),COEFF=mean(Output$Store[11,6]),
                  SQ=mean(Output$Store[11,7]),SMSC=mean(Output$Store[11,8]),
                  SUB=mean(Output$Store[11,9]),CRAK=mean(Output$Store[11,10]),
                  K=mean(Output$Store[11,11]),SMSC=mean(Output$Store[11,12]),
                  etmult=mean(Output$Store[11,13]),
                  return_state=F)
  }
  # now put in pred.data
  pred.mod <- predict(Mod,newdata=pred.data, all=T,na.rm=F)
  plot(pred.data$Q-pred.mod)
  lines(time(pred.data),rep(0,length=length(pred.data$Q)),lty=2,col="red",lwd=2)
  # maybe first summarise to weekly data?
  #resid.weekly <- apply.weekly(pred.data$Q-pred.mod,mean)
  #test <- data.frame(resid=resid.weekly,trend=1:length(resid.weekly))
  # Create the residual data set
  test <- data.frame(resid=pred.data$Q-pred.mod,trend=1:length(pred.data$Q))
  
  mod.test <- gam(resid~trend,data=test,na.action=na.omit,correlation=corCAR1())
  Results[i,2:4] <- c(as.numeric(summary(mod.test)$p.table[2,c(1,4)]),
                      as.numeric(summary(mod.test)$r.sq))

  MKout <- MannKendall(test$resid)
  Results[i,5:6] <- as.numeric(MKout[1:2])
  
  Results[i,7:8] <- c(summary(Mod)$r.squared,summary(Mod)$rel.bias)
}
Results
