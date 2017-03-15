#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# bootstrap MK test, deseasonalise the weekly data
# this version: also do a gamm linear trend analysis
# version from 20150830: flow data converted from ML/day to mm
# need to load the raw data as this needs to be deseasonalise


# INPUT FILES:
# - Flow station names
# - Catchment characteristics 20150829_CatchmentCharact.csv
# - individual station, rain and maxT data

##################
##  ~ Set up ~  ##
##################
# SET WORKING DIRECTORY # #####
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")
Today <- format(Sys.Date(),"%Y%m%d")



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
# # FLOW, RAINFALL AND TEMPERATURE DATA # #####
 flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004",
                         "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B",
                         "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497",
                         "MURR"="405205", "SOUT"="225020A", "YARR"="614044",
                         "DOMB"="607155")
 # Flow data
load("ProjectData\\20160726_ClimCh_project_MD.Rdata")

require(deseasonalize)
# deseasonalise
#test <- ds(na.omit(rainfall_weekly_xts[,1]),ic="AIC")
# flow
flow_deseas <- flow_zoo
# now assign to a new dataframe
for (i in (seq_along(flow_stns))) {
  foo <- flow_zoo[,i]
# replacce NA values with mean flow
  bad <- is.na(foo) 
  foo[bad] <- mean(foo,na.rm=T)
  flow_deseas[,i]  <- ds(as.ts(foo),ic="AIC")$z
# put NA values back
  flow_deseas[bad,i] <- NA
}
# rainfall
rain_deseas <- rain_zoo
# now assign to a new dataframe
for (i in seq_along(flow_stns)) {
  foo <- rain_zoo[,i]
  # replacce NA values with mean flow
  bad <- is.na(foo) 
  foo[bad] <- mean(foo,na.rm=T)
  rain_deseas[,i]  <- ds(as.numeric(foo),ic="AIC")$z
  # put NA values back
  rain_deseas[bad,i] <- NA
}

# maximum temperature
maxT_deseas <- maxT_zoo
# now assign to a new dataframe
for (i in seq_along(flow_stns)) {
  foo <- maxT_zoo[,i]
  # replacce NA values with mean flow
  bad <- is.na(foo) 
  foo[bad] <- mean(foo,na.rm=T)
  maxT_deseas[,i]  <- ds(as.numeric(foo),ic="AIC")$z
  # put NA values back
  maxT_deseas[bad,i] <- NA
}

# -----------------------------------
# xts and weekly and monthly data
# -----------------------------
flow_xts <- xts(flow_deseas, 
                order.by=time(flow_deseas), 
                frequency=1)
flow_weekly_xts <- apply.weekly(flow_xts, mean)
flow_monthly_xts <- apply.monthly(flow_xts,mean)
# weekly_flow_stack <- as.data.frame(flow_weekly_xts)
# weekly_flow_stack <- data.frame(Date=row.names(weekly_flow_stack), weekly_flow_stack)
# colnames(weekly_flow_stack) <- c("Date", "flow.COTT", "flow.RUTH", "flow.CORA", "flow.ELIZ", "flow.COCH", "flow.COEN", "flow.SCOT", "flow.HELL", "flow.NIVE", "flow.MURR", "flow.SOUT", "flow.YARR", "flow.DOMB")
# weekly_flow_stack <- reshape(weekly_flow_stack, direction="long", varying=2:14, sep=".")
rainfall_xts <- xts(rain_deseas, 
                    order.by=time(rain_deseas), 
                    frequency=1)
rainfall_weekly_xts <- apply.weekly(rainfall_xts, mean)
rainfall_monthly_xts <- apply.monthly(rainfall_xts,mean)
# weekly_rain_stack <- as.data.frame(rainfall_weekly_xts)
# colnames(weekly_rain_stack) <- c("rain.COTT", "rain.RUTH", "rain.CORA", "rain.ELIZ", "rain.COCH", "rain.COEN", "rain.SCOT", "rain.HELL", "rain.NIVE", "rain.MURR", "rain.SOUT", "rain.YARR", "rain.DOMB")
# weekly_rain_stack <- reshape(weekly_rain_stack, direction="long", varying=1:13, sep=".")
maxT_xts <- xts(maxT_deseas, 
                order.by=time(maxT_deseas), 
                frequency=1)
# this calculates the "mean" maximum temperature
maxT_weekly_xts <- apply.weekly(maxT_xts, mean)
maxT_monthly_xts <- apply.monthly(maxT_xts,mean)
# Maybe should calculate the "max" maximum temperature
m.maxT_weekly_xts <- apply(maxT_xts,2,function(x) apply.weekly(x,max,na.rm=T))
m.maxT_monthly_xts <- apply.monthly(maxT_xts,function(x) max(x,na.rm=T))

# weekly_maxT_stack <- as.data.frame(maxT_weekly_xts)
# colnames(weekly_maxT_stack) <- c("maxT.COTT", "maxT.RUTH", "maxT.CORA", "maxT.ELIZ", "maxT.COCH", "maxT.COEN", "maxT.SCOT", "maxT.HELL", "maxT.NIVE", "maxT.MURR", "maxT.SOUT", "maxT.YARR", "maxT.DOMB")
# weekly_maxT_stack <- reshape(weekly_maxT_stack, direction="long", varying=1:13, sep=".")
# flow_rain_maxT_weekly <- data.frame(Date=weekly_flow_stack$Date, Station=weekly_flow_stack$time, Flow=weekly_flow_stack$flow, Rain=weekly_rain_stack$rain, MaxT=weekly_maxT_stack$maxT)
# # Remove NAs
# flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)

# -----------------------------
# set up the bootstrap for the weekly data for MannKendall
# Basically swap out the years
# -----------------------
# first split the data set
split.flow <- split(flow_weekly_xts,"years")
split.rain <- split(rainfall_weekly_xts,"years")
split.maxT <- split(maxT_weekly_xts,"years")

# Bootstrap
set.seed(10)
# now run a loop over the number of years (create 41 different sets)
# do Mann Kendall test on each resonstituted series
# ---------------------------
# Streamflow
# -------------------------

MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(flow_weekly_xts),nyears(flow_weekly_xts))
  for (j in 1:length(series)) {
   # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.flow[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.flow[[series[j]]]))
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
  sig.set[[i]] <- set[set$pvalue < 0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(flow_weekly_xts,2,MannKendall))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp <- hp + facet_wrap(~ catch,ncol=5) + ggtitle("Streamflow")
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp <- hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + scale_colour_grey(start = 0, end = 0.6) +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Deseasonalised Streamflow") #+

save(hp,file=paste(Today,"_MKdeseasonalisedStreamflow.Rdata",sep=""))
load("20160729_MKdeseasonalisedStreamflow.Rdata")
# draft quality
#tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
#          Today,"_MKdeseasonalisedStreamflow.tif",sep=""),width=720,height=480)
#print(hp)
dev.off()     
     
# publication quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/Manuscript",
           "MKdeseasonalisedStreamflow.tif",sep="/"),width=12*480,height=10*480,
     compression = "lzw", res=600)
print(hp)
dev.off()
# -------- end flow -----------------


# -------------------------
# rainfall
# ------------------------
MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(rainfall_weekly_xts),nyears(rainfall_weekly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.rain[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.rain[[series[j]]]))
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
  sig.set[[i]] <- set[set$pvalue< 0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(rainfall_weekly_xts,2,MannKendall))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp <- hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp <- hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Deseasonalised Rainfall") #+
print(hp)

# -----end rainfall -------------------


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
  facet_wrap(~ catch,ncol=5)+ ggtitle("Deseasonalised Maximum Temperature") #+
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
  facet_wrap(~ catch,ncol=5)+ ggtitle("Maximum Maximum Temperature") #+
# ++++++++++ MONTHLY +++++++++++++++++++++++++++
# set up the bootstrap for the monthly data for Seasonal
# MannKendall
# Basically swap out the years
# -----------------------
# first split the data set
split.flow <- split(flow_monthly_xts,"years")
split.rain <- split(rainfall_monthly_xts,"years")
split.maxT <- split(maxT_monthly_xts,"years")

# Bootstrap
set.seed(10)

# now run a loop over the number of years (create 41 different sets)
# do Seasonal Mann Kendall test on each resonstituted series
# ++++++++++++++++++++++++++++++++++++++++++++++++++
# ---------------------------
# Streamflow
# -------------------------


MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(flow_monthly_xts),nyears(flow_monthly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.flow[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.flow[[series[j]]]))
    }
    #new.list[[j]] <- split.flow[[series[j]]]
  }
  # rbind to dataframe
  #if(i == 1) plot(new.df[,1], type="l") else lines(new.df[,1],col=i)
  # run mann kendall on the columns and store the results
  mk.r <- apply(new.df,2,function(x) SeasonalMannKendall(as.ts(x)))
  
  MK.list[[i]] <- do.call(cbind,mk.r)  
}

MK.df <- do.call(rbind,MK.list)
#hist(as.numeric(MK.df[row.names(MK.df)=="tau","COEN"]))

pvalues <- subset(MK.df, rownames(MK.df)=="sl")
tau <- subset(MK.df, rownames(MK.df)=="tau")

require(ggplot2)
sig.set <- list()

for (i in 1:ncol(pvalues)) {
  #i <- 1
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
                    tau=as.numeric(tau[,i]),catch=rep(colnames(MK.df)[i],nrow(tau)))
  sig.set[[i]] <- set[set$pvalue < 0.05,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))
real <- do.call(rbind,apply(flow_monthly_xts,2,
                            function(x) SeasonalMannKendall(as.ts(x))))
real.df <- data.frame(pvalue = as.numeric(real[,2]), 
                      tau = as.numeric(real[,1]),
                      catch=rownames(real),type=rep("real",nrow(real)))
sig.set.f <- rbind(sig.set.a,real.df)
# A histogram of taus
hp <- ggplot(sig.set.a, aes(x=tau)) + geom_histogram(binwidth=0.03,colour="white")

# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_wrap(~ catch,ncol=5) + ggtitle("Monthly Streamflow")
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Monthly Streamflow") #+
# -------- end flow -----------------


# -------------------------
# rainfall
# ------------------------
MK.list <- list()
for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(rainfall_monthly_xts),
                   nyears(rainfall_monthly_xts))
  for (j in 1:length(series)) {
    # j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.rain[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.rain[[series[j]]]))
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
real <- do.call(rbind,apply(rainfall_monthly_xts,2,
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
  facet_wrap(~ catch,ncol=5)+ ggtitle("Monthly Rainfall") #+
# -----end rainfall -------------------


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
  facet_wrap(~ catch,ncol=5)+ ggtitle("Monthly Maximum Temperature") #+
#scale_colour_discrete(name="p-value",
#                    breaks=c("darkblue","red"),
#                    labels=c("<0.1",">=0.1"))




