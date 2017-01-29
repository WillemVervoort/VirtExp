#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# bootstrap MK test
# THIS VERSION: 
# Running only temperature analysis on the anomalies

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
#setwd("/Users/michaeladolk/Desktop/Streamflow_data")
#setwd("U:/My-Workspace/mdol7996/Streamflow_2015")
#setwd("C:/Users/User/Desktop/Michaela/Week 4")
#####
# LOAD REQUIRED PACKAGES # #####
library(xts)
library(zoo)
library(Kendall)
library(mgcv)
library(oz)
require(ggplot2)
#####
# STATIONS # #####
#flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
#####
# TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")
subset_function <- function(x) subset(x, as.Date(Date, format="%d/%m/%y")>=start_date & as.Date(Date, format="%d/%m/%y")<=end_date)

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
  assign(paste(colnames(flow_stns[i]), "temp.maxT", sep=""), as.zoo(x=temp$maxT, order.by=temp$Date))
  print(i)
}
temp.maxT<-merge(COTTtemp.maxT,RUTHtemp.maxT,CORAtemp.maxT,ELIZtemp.maxT,COCHtemp.maxT,COENtemp.maxT,SCOTtemp.maxT,HELLtemp.maxT,NIVEtemp.maxT,MURRtemp.maxT,SOUTtemp.maxT,YARRtemp.maxT,DOMBtemp.maxT)
temp.maxT<-as.data.frame(temp.maxT)
# Calculate anomalies and store
maxT_data_70_10<-data.frame(Date=row.names(temp.maxT), 
            COTT=temp.maxT$COTTtemp.maxT - mean(temp.maxT$COTTtemp.maxT,na.rm=T),
            RUTH=temp.maxT$RUTHtemp.maxT - mean(temp.maxT$RUTHtemp.maxT,na.rm=T),
            CORA=temp.maxT$CORAtemp.maxT - mean(temp.maxT$CORAtemp.maxT,na.rm=T),
            ELIZ=temp.maxT$ELIZtemp.maxT - mean(temp.maxT$ELIZtemp.maxT,na.rm=T),
            COCH=temp.maxT$COCHtemp.maxT - mean(temp.maxT$COCHtemp.maxT,na.rm=T),
            COEN=temp.maxT$COENtemp.maxT - mean(temp.maxT$COENtemp.maxT,na.rm=T),
            SCOT=temp.maxT$SCOTtemp.maxT - mean(temp.maxT$SCOTtemp.maxT,na.rm=T),
            HELL=temp.maxT$HELLtemp.maxT - mean(temp.maxT$HELLtemp.maxT,na.rm=T),
            NIVE=temp.maxT$NIVEtemp.maxT - mean(temp.maxT$NIVEtemp.maxT,na.rm=T),
            MURR=temp.maxT$MURRtemp.maxT - mean(temp.maxT$MURRtemp.maxT,na.rm=T),
            SOUT=temp.maxT$SOUTtemp.maxT - mean(temp.maxT$SOUTtemp.maxT,na.rm=T),
            YARR=temp.maxT$YARRtemp.maxT - mean(temp.maxT$YARRtemp.maxT,na.rm=T),
            DOMB=temp.maxT$DOMBtemp.maxT - mean(temp.maxT$DOMBtemp.maxT,na.rm=T))

# -----------------------------------
# xts and weekly and monthly data
# -----------------------------
maxT_xts <- xts(maxT_data_70_10[2:14], 
                order.by=as.Date(maxT_data_70_10$Date), 
                frequency=1)
# this calculates the "mean" maximum temperature
maxT_weekly_xts <- apply.weekly(maxT_xts, mean)
maxT_monthly_xts <- apply.monthly(maxT_xts,mean)
# Maybe should calculate the "max" maximum temperature
m.maxT_weekly_xts <- apply.weekly(maxT_xts,function(x) as.numeric(apply(x,2,max)))
m.maxT_monthly_xts <- apply.monthly(maxT_xts,function(x) max(x))

# weekly_maxT_stack <- as.data.frame(maxT_weekly_xts)
# colnames(weekly_maxT_stack) <- c("maxT.COTT", "maxT.RUTH", "maxT.CORA", "maxT.ELIZ", "maxT.COCH", "maxT.COEN", "maxT.SCOT", "maxT.HELL", "maxT.NIVE", "maxT.MURR", "maxT.SOUT", "maxT.YARR", "maxT.DOMB")
# weekly_maxT_stack <- reshape(weekly_maxT_stack, direction="long", varying=1:13, sep=".")
# flow_rain_maxT_weekly <- data.frame(Date=weekly_flow_stack$Date, Station=weekly_flow_stack$time, Flow=weekly_flow_stack$flow, Rain=weekly_rain_stack$rain, MaxT=weekly_maxT_stack$maxT)
# # Remove NAs
# flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)
#####

# -----------------------------
# set up the bootstrap for the weekly data for MannKendall
# Basically swap out the years
# -----------------------
# Bootstrap
set.seed(10)

split.maxT <- split(maxT_weekly_xts,"years")

# now run a loop over the number of years (create 41 different sets)
# do Mann Kendall test on each resonstituted series
# ------------------
#  Mean Maximum temperature
# ---------------------
MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(maxT_weekly_xts),nyears(maxT_weekly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.maxT[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.maxT[[series[j]]]))
    }
    #new.list[[j]] <- split.flow[[series[j]]]
  }
  # rbind to dataframe
  #if(i == 1) plot(new.df[,1], type="l") else lines(new.df[,1],col=i)
  # run mann kendall on the columns and store the results
  mk.r <- apply(new.df,2,MannKendall)
  
  MK.list[[i]] <- do.call(cbind,mk.r)  
}

MK.df <- do.call(rbind,MK.list)
#hist(as.numeric(MK.df[row.names(MK.df)=="tau","COEN"]))

pvalues <- subset(MK.df, rownames(MK.df)=="sl")
tau <- subset(MK.df, rownames(MK.df)=="tau")

sig.set <- list()

for (i in 1:ncol(pvalues)) {
  #i <- 1
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
                    tau=as.numeric(tau[,i]),catch=rep(colnames(MK.df)[i],nrow(tau)))
  sig.set[[i]] <- set[set$pvalue<0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(maxT_weekly_xts,2,MannKendall))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + 
  geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.1,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Anomalies Maximum Temperature") #+
  #scale_colour_discrete(name="p-value",
  #                    breaks=c("darkblue","red"),
  #                    labels=c("<0.1",">=0.1"))
# ------------------
#  Maximum Maximum temperature
# ---------------------
split.maxT <- split(m.maxT_weekly_xts,"years")


MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(m.maxT_weekly_xts),nyears(m.maxT_weekly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.maxT[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.maxT[[series[j]]]))
    }
    #new.list[[j]] <- split.flow[[series[j]]]
  }
  # rbind to dataframe
  #if(i == 1) plot(new.df[,1], type="l") else lines(new.df[,1],col=i)
  # run mann kendall on the columns and store the results
  mk.r <- apply(new.df,2,MannKendall)
  
  MK.list[[i]] <- do.call(cbind,mk.r)  
}

MK.df <- do.call(rbind,MK.list)
#hist(as.numeric(MK.df[row.names(MK.df)=="tau","COEN"]))

pvalues <- subset(MK.df, rownames(MK.df)=="sl")
tau <- subset(MK.df, rownames(MK.df)=="tau")

sig.set <- list()

for (i in 1:ncol(pvalues)) {
  #i <- 1
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
                    tau=as.numeric(tau[,i]),catch=rep(colnames(MK.df)[i],nrow(tau)))
  sig.set[[i]] <- set[set$pvalue<0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(m.maxT_weekly_xts,2,MannKendall))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + 
  geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.1,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Maximum anomalies Maximum Temperature") #+
# ++++++++++ MONTHLY +++++++++++++++++++++++++++
# set up the bootstrap for the monthly data for Seasonal
# MannKendall
# Basically swap out the years
# -----------------------
# first split the data set
split.maxT <- split(maxT_monthly_xts,"years")

# Bootstrap
set.seed(10)

# now run a loop over the number of years (create 41 different sets)
# do Seasonal Mann Kendall test on each resonstituted series
# ++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------------
#  Maximum temperature
# ---------------------
MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(maxT_monthly_xts),
                   nyears(maxT_monthly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.maxT[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.maxT[[series[j]]]))
    }
    #new.list[[j]] <- split.flow[[series[j]]]
  }
  # rbind to dataframe
  #if(i == 1) plot(new.df[,1], type="l") else lines(new.df[,1],col=i)
  # run mann kendall on the columns and store the results
  mk.r <- apply(new.df,2,
                function(x) SeasonalMannKendall(as.ts(x)))
  
  MK.list[[i]] <- do.call(cbind,mk.r)  
}

MK.df <- do.call(rbind,MK.list)
#hist(as.numeric(MK.df[row.names(MK.df)=="tau","COEN"]))

pvalues <- subset(MK.df, rownames(MK.df)=="sl")
tau <- subset(MK.df, rownames(MK.df)=="tau")

sig.set <- list()

for (i in 1:ncol(pvalues)) {
  #i <- 1
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
              tau=as.numeric(tau[,i]),catch=rep(colnames(MK.df)[i],nrow(tau)))
  sig.set[[i]] <- set[set$pvalue < 0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(maxT_monthly_xts,2,
                            function(x) SeasonalMannKendall(as.ts(x))))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + 
  geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Monthly Anomalies Maximum Temperature") #+
#scale_colour_discrete(name="p-value",
#                    breaks=c("darkblue","red"),
#                    labels=c("<0.1",">=0.1"))
# first split the data set
split.maxT <- split(m.maxT_monthly_xts,"years")

# Bootstrap
set.seed(10)

# now run a loop over the number of years (create 41 different sets)
# do Seasonal Mann Kendall test on each resonstituted series
# ++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------------
#  Maximum Maximum temperature
# ---------------------
MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(maxT_monthly_xts),
                   nyears(maxT_monthly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.maxT[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.maxT[[series[j]]]))
    }
    #new.list[[j]] <- split.flow[[series[j]]]
  }
  # rbind to dataframe
  #if(i == 1) plot(new.df[,1], type="l") else lines(new.df[,1],col=i)
  # run mann kendall on the columns and store the results
  mk.r <- apply(new.df,2,
                function(x) SeasonalMannKendall(as.ts(x)))
  
  MK.list[[i]] <- do.call(cbind,mk.r)  
}

MK.df <- do.call(rbind,MK.list)
#hist(as.numeric(MK.df[row.names(MK.df)=="tau","COEN"]))

pvalues <- subset(MK.df, rownames(MK.df)=="sl")
tau <- subset(MK.df, rownames(MK.df)=="tau")

sig.set <- list()

for (i in 1:ncol(pvalues)) {
  #i <- 1
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
                    tau=as.numeric(tau[,i]),catch=rep(colnames(MK.df)[i],nrow(tau)))
  sig.set[[i]] <- set[set$pvalue < 0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(maxT_monthly_xts,2,
                            function(x) SeasonalMannKendall(as.ts(x))))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + 
  geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Monthly Anomalies Maximum Temperature") #+
#scale_colour_discrete(name="p-value",
#                    breaks=c("darkblue","red"),
#                    labels=c("<0.1",">=0.1"))
