#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# This script is to analyse the results of the HPC experiment

setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")

require(zoo)
require(xts)
require(hydromad)
require(mgcv)
require(Kendall)
require(Rcpp)
Today <- format(Sys.Date(),"%Y%m%d")

load("20160726_ClimCh_project_MD.Rdata")
source("C:/Users/rver4657/ownCloud/Virtual Experiments/ProjectRCode/Simhyd.r")


start.date <- as.Date("1981-01-01")
end.date <- as.Date("2010-12-31")
# Flow stations
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", 
                        "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", 
                        "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497",
                        "MURR"="405205", "SOUT"="225020A", "YARR"="614044", 
                        "DOMB"="607155")
sum.Res <- list()
Chiew.Res <- list()
# storage for Chiew results
Chiew <- data.frame(station=character(length=10),eta_p=numeric(length=10),
                    eta_e=numeric(length=10))
# Storage for residual analysis
Results <- data.frame(station=character(length=10),trend=numeric(length=10),
                      p_trend=numeric(length=10),r_sq=numeric(length=10),
                      tau=numeric(length=10),p_tau=numeric(length=10),
                      Mod.r.sq=numeric(length=10), Mod.bias=numeric(length=10))

# read in the modelling results
for (i in seq_along(flow_stns)) {
  # first test one

  #i <- 1
  # load the rainfall, ET and flow data
  pred.data <- window(merge(flow_zoo[,i], rain_zoo[,i], maxT_zoo[,i]),
                      start=start.date, end=end.date)
  colnames(pred.data) <- c("Q","P","E")
  
  # load the relevant output
  load(paste("HPC/MD_ProjectData/20160727_",names(flow_stns)[i],"CalibOutput.Rdata",sep=""))
  # extract the model and update with the parameters
  Mod <- Output$mod
  
  resid.out <- data.frame(matrix(0,ncol=10,nrow=nrow(pred.data)))
  # check for minimum obj function
  Chiew[,1] <- colnames(flow_stns)[i]
  Results[,1] <- colnames(flow_stns)[i]
  
  
  
  for (j in 1:nrow(Output$Store)) {
    # testing
    #j <- 1
    # make model
    Mod <- update(Mod, INSC=Output$Store[j,5],COEFF=Output$Store[j,6],
                      SQ=Output$Store[j,7],SMSC=Output$Store[j,8],
                      SUB=Output$Store[j,9],CRAK=Output$Store[j,10],
                      K=Output$Store[j,11],
                    etmult=Output$Store[j,12], DELAY=Output$Store[j,13],
                    X_m = Output$Store[j,14],
                  return_state=F)
    

    # now put in pred.data
    pred.mod <- predict(Mod,newdata=pred.data, all=T,na.rm=F)
    if (j==1) {
      plot(pred.data$Q-pred.mod)
      lines(time(pred.data),rep(0,length=length(pred.data$Q)),lty=2,col="red",lwd=2)
    } else {
      lines(pred.data$Q-pred.mod,col=j+1)
    }
    # maybe first summarise to weekly data?
    #resid.weekly <- apply.weekly(pred.data$Q-pred.mod,mean)
    #test <- data.frame(resid=resid.weekly,trend=1:length(resid.weekly))
    # Create the residual data set
    test <- data.frame(resid=pred.data$Q-pred.mod,trend=1:length(pred.data$Q))
    
    mod.test <- gam(resid~trend,data=test,na.action=na.omit,correlation=corCAR1())
    Results[j,2:4] <- c(as.numeric(summary(mod.test)$p.table[2,c(1,4)]),
                        as.numeric(summary(mod.test)$r.sq))
  
    MKout <- MannKendall(test$resid)
    Results[j,5:6] <- as.numeric(MKout[1:2])
    
    Results[j,7:8] <- c(summary(Mod)$r.squared,summary(Mod)$rel.bias)
    
    # Now run the Chiew 2006 simulations on all the data
    pred.results <- data.frame(Pmin15ET0=numeric(length=nrow(flow_zoo)),
                                  Pmin10ET0=numeric(length=nrow(flow_zoo)),
                                  P0ET0=numeric(length=nrow(flow_zoo)),
                                  Pplus10ET0=numeric(length=nrow(flow_zoo)),
                                  Pmin15ETplus5=numeric(length=nrow(flow_zoo)),
                                  Pmin10ETplus5=numeric(length=nrow(flow_zoo)),
                                  P0ETplus5=numeric(length=nrow(flow_zoo)),
                                  Pplus10ETplus5=numeric(length=nrow(flow_zoo)),
                                  Pmin15ETplus10=numeric(length=nrow(flow_zoo)),
                                  Pmin10ETplus10=numeric(length=nrow(flow_zoo)), 
                                  P0ETplus10=numeric(length=nrow(flow_zoo)),
                                  Pplus10ETplus10=numeric(length=nrow(flow_zoo)))
mu <- cbind(rep(c(-15,-10,0,10),3),c(rep(0,4),rep(5,4),rep(10,4)))
    # Create the precipitation and ET data variations
    test <- list()
    for (k in 1:nrow(mu)) {
      temp <- as.data.frame(cbind((1+mu[k,1]/100)*rain_zoo[,i],
                             (1+mu[k,2]/100)*maxT_zoo[,i]))
      test[[k]] <- do.call(cbind,apply(temp,2,function(x) aggregate(x,
                        list(year=format(time(flow_zoo),"%Y")),sum,na.rm=T)))
      test[[k]] <- test[[k]][,-3]
    }
    clim.adj <- do.call(rbind,test)
    
    # now run the different pred results
    for (k in 1:ncol(pred.results)) {
      # run the model over all data
      pred.data2 <- window(merge(flow_zoo[,i], 
                                 (1+mu[k,1]/100)*rain_zoo[,i], 
                                 (1+mu[k,2]/100)*maxT_zoo[,i]))
      colnames(pred.data2) <- c("Q","P","E")
      
      pred.results[,k] <- predict(Mod,newdata=pred.data2, all=T,na.rm=F)
    }
    # summarise the data annually
    pred.ann <- apply(pred.results,2,function(x) aggregate(x,list(year=format(time(flow_zoo),"%Y")),sum,na.rm=T))
    ann_flow <- rep(pred.ann[[1]][,2],6)
    pred.t <- do.call(rbind,pred.ann)
    # Now add the ET and precipitation data
    pred.ann <- data.frame(pred.t,rain=clim.adj[,2],maxT=clim.adj[,3])
    # summarise base rain and temp
    ann_rain <- rep(aggregate(rain_zoo[,i],list(year=format(time(flow_zoo),"%Y")),sum,na.rm=T),6)
    ann_maxT <- rep(aggregate(maxT_zoo[,i],list(year=format(time(flow_zoo),"%Y")),sum,na.rm=T),6)

    # Now calculate the difference
    pred.diff <- pred.ann
    pred.diff[,2] <- pred.diff[,2] - ann_flow
    pred.diff[,3] <- pred.diff[,3] - ann_rain
    pred.diff[,4] <- pred.diff[,4] - ann_maxT
    
    fit <- lm(x~rain + maxT,data=pred.diff)
    
    Chiew[j,2:3] <- coef(fit)[2:3]
    
    
  }
  Chiew.Res[[i]] <- Chiew
  sum.Res[[i]] <- Results
}

OutputTrends <- do.call(rbind,sum.Res)
write.csv(OutputTrends,file=paste(Today,"_SimHydHPC_trendsAnalysis.csv",sep=""),
          row.names=F)

OutputChiew <- do.call(rbind,Chiew.Res)
write.csv(OutputChiew,file=paste(Today,"_SimHydHPC_ChiewAnalysis.csv",sep=""), 
          row.names=F)

