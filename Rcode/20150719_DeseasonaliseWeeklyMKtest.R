#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# bootstrap MK test, deseasonalise the weekly data

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
# FLOW, RAINFALL AND TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
# Flow data
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""),
         read.csv(paste(flow_stns[,i],"_daily_ts2.csv", sep=""), 
                  col.names=c("Date", "Q")))
}
flow_df_list <- list(COTT=COTT_daily_flow, RUTH=RUTH_daily_flow, CORA=CORA_daily_flow, ELIZ=ELIZ_daily_flow, COCH=COCH_daily_flow, COEN=COEN_daily_flow, SCOT=SCOT_daily_flow, HELL=HELL_daily_flow, NIVE=NIVE_daily_flow, MURR=MURR_daily_flow, SOUT=SOUT_daily_flow, YARR=YARR_daily_flow, DOMB=DOMB_daily_flow)
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")
subset_function <- function(x) subset(x, as.Date(Date, format="%d/%m/%y")>=start_date & as.Date(Date, format="%d/%m/%y")<=end_date)
flow_df_list <- lapply(flow_df_list, subset_function)
flow_data_70_10 <- data.frame(Date=flow_df_list$COTT$Date, COTT=flow_df_list$COTT$Q,
                              RUTH=flow_df_list$RUTH$Q, CORA=flow_df_list$CORA$Q,
                              ELIZ=flow_df_list$ELIZ$Q, COCH=flow_df_list$COCH$Q,
                              COEN=flow_df_list$COEN$Q, SCOT=flow_df_list$SCOT$Q,
                              HELL=flow_df_list$HELL$Q, NIVE=flow_df_list$NIVE$Q,
                              MURR=flow_df_list$MURR$Q, SOUT=flow_df_list$SOUT$Q,
                              YARR=flow_df_list$YARR$Q, DOMB=flow_df_list$DOMB$Q)
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
temp<-merge(COTTtemp,RUTHtemp,CORAtemp,ELIZtemp,
            COCHtemp,COENtemp,SCOTtemp,HELLtemp,
            NIVEtemp,MURRtemp,SOUTtemp,YARRtemp,
            DOMBtemp,all=T)
temp<-as.data.frame(temp)
rainfall_data_70_10<-data.frame(Date=row.names(temp), COTT=temp$COTTtemp,
                                RUTH=temp$RUTHtemp, CORA=temp$CORAtemp,
                                ELIZ=temp$ELIZtemp, COCH=temp$COCHtemp,
                                COEN=temp$COENtemp, SCOT=temp$SCOTtemp,
                                HELL=temp$HELLtemp, NIVE=temp$NIVEtemp,
                                MURR=temp$MURRtemp, SOUT=temp$SOUTtemp,
                                YARR=temp$YARRtemp, DOMB=temp$DOMBtemp)
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
maxT_data_70_10<-data.frame(Date=row.names(temp.maxT), 
            COTT=temp.maxT$COTTtemp.maxT,
            RUTH=temp.maxT$RUTHtemp.maxT,
            CORA=temp.maxT$CORAtemp.maxT,
            ELIZ=temp.maxT$ELIZtemp.maxT,
            COCH=temp.maxT$COCHtemp.maxT,
            COEN=temp.maxT$COENtemp.maxT,
            SCOT=temp.maxT$SCOTtemp.maxT,
            HELL=temp.maxT$HELLtemp.maxT,
            NIVE=temp.maxT$NIVEtemp.maxT,
            MURR=temp.maxT$MURRtemp.maxT,
            SOUT=temp.maxT$SOUTtemp.maxT,
            YARR=temp.maxT$YARRtemp.maxT,
            DOMB=temp.maxT$DOMBtemp.maxT)
#####
require(deseasonalize)
# deseasonalise
#test <- ds(na.omit(rainfall_weekly_xts[,1]),ic="AIC")
# flow
flow_deseas <- flow_data_70_10
# now assign to a new dataframe
for (i in (seq_along(flow_stns))) {
  foo <- flow_data_70_10[,i+1]
# replacce NA values with mean flow
  bad <- is.na(foo) 
  foo[bad] <- mean(foo,na.rm=T)
  flow_deseas[,colnames(flow_stns[i])]  <- ds(foo,ic="AIC")$z
# put NA values back
  flow_deseas[bad,colnames(flow_stns[i])] <- NA
}
# rainfall
rain_deseas <- rainfall_data_70_10
# now assign to a new dataframe
for (i in seq_along(flow_stns)) {
  foo <- rainfall_data_70_10[,i+1]
  # replacce NA values with mean flow
  bad <- is.na(foo) 
  foo[bad] <- mean(foo,na.rm=T)
  rain_deseas[,colnames(flow_stns[i])]  <- ds(foo,ic="AIC")$z
  # put NA values back
  rain_deseas[bad,colnames(flow_stns[i])] <- NA
}

# maximum temperature
maxT_deseas <- maxT_data_70_10
# now assign to a new dataframe
for (i in seq_along(flow_stns)) {
  foo <- maxT_data_70_10[,i+1]
  # replacce NA values with mean flow
  bad <- is.na(foo) 
  foo[bad] <- mean(foo,na.rm=T)
  maxT_deseas[,colnames(flow_stns[i])]  <- ds(foo,ic="AIC")$z
  # put NA values back
  maxT_deseas[bad,colnames(flow_stns[i])] <- NA
}

# -----------------------------------
# xts and weekly and monthly data
# -----------------------------
flow_xts <- xts(flow_deseas[2:14], 
                order.by=as.Date(rain_deseas$Date), 
                frequency=1)
flow_weekly_xts <- apply.weekly(flow_xts, mean)
flow_monthly_xts <- apply.monthly(flow_xts,mean)
# weekly_flow_stack <- as.data.frame(flow_weekly_xts)
# weekly_flow_stack <- data.frame(Date=row.names(weekly_flow_stack), weekly_flow_stack)
# colnames(weekly_flow_stack) <- c("Date", "flow.COTT", "flow.RUTH", "flow.CORA", "flow.ELIZ", "flow.COCH", "flow.COEN", "flow.SCOT", "flow.HELL", "flow.NIVE", "flow.MURR", "flow.SOUT", "flow.YARR", "flow.DOMB")
# weekly_flow_stack <- reshape(weekly_flow_stack, direction="long", varying=2:14, sep=".")
rainfall_xts <- xts(rain_deseas[2:14], 
                    order.by=as.Date(rain_deseas$Date), 
                    frequency=1)
rainfall_weekly_xts <- apply.weekly(rainfall_xts, mean)
rainfall_monthly_xts <- apply.monthly(rainfall_xts,mean)
# weekly_rain_stack <- as.data.frame(rainfall_weekly_xts)
# colnames(weekly_rain_stack) <- c("rain.COTT", "rain.RUTH", "rain.CORA", "rain.ELIZ", "rain.COCH", "rain.COEN", "rain.SCOT", "rain.HELL", "rain.NIVE", "rain.MURR", "rain.SOUT", "rain.YARR", "rain.DOMB")
# weekly_rain_stack <- reshape(weekly_rain_stack, direction="long", varying=1:13, sep=".")
maxT_xts <- xts(maxT_deseas[2:14], 
                order.by=as.Date(maxT_deseas$Date), 
                frequency=1)
# this calculates the "mean" maximum temperature
maxT_weekly_xts <- apply.weekly(maxT_xts, mean)
maxT_monthly_xts <- apply.monthly(maxT_xts,mean)
# Maybe should calculate the "max" maximum temperature
m.maxT_weekly_xts <- apply.weekly(maxT_xts,function(x) as.numeric(apply(x,2,max,na.rm=T)))
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
hp + facet_wrap(~ catch,ncol=5) + ggtitle("Streamflow")
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Deseasonalised Streamflow") #+
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
hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Deseasonalised Rainfall") #+
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
