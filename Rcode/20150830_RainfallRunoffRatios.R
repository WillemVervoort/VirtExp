## 
# Script to calculate annual and total rainfall and flow

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
#####
# STATIONS # #####
#####
# FLOW, RAINFALL AND TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
# Flow data
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""),
         read.csv(paste(flow_stns[,i],"_daily_ts2.csv", sep=""), 
                  col.names=c("Date", "Q")))
}

# read in catchment characteristics to get sizes
CC <- read.csv("20150829_CatchmentCharact.csv")

# Write flow files as zoo
for (i in seq_along(flow_stns)) {
  temp <- get(paste(colnames(flow_stns[i]), "_daily_flow", sep=""))
  year <- substr(as.character(temp$Date),nchar(as.character(temp$Date))-1,nchar(as.character(temp$Date)))
  Dates <- as.Date(paste(substr(as.character(temp$Date),1,nchar(as.character(temp$Date))-2),
                         ifelse(as.numeric(year)>=50,paste("19",year,sep=""),paste("20",year,sep="")),sep=""),
                   "%d/%m/%Y")
  
  # convert ML/day to mm
  # km2 --> ha = x100  ML/day ha to mm = flow/ha/100. so flow/area = mm
  assign(paste(colnames(flow_stns[i]), "_daily_flow.z", sep=""),
         zoo(temp$Q/CC[i,4],order.by=Dates))
}

#head(COTT_daily_flow.z)
# merge all flow data and subset
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

# calculate annual totals for flow 
totals <- apply(flow_data,2,sum)

annual <- aggregate(flow_data,format(time(flow_data),"%Y"),sum,na.rm=T)
mean_annual <- apply(annual,2,mean,na.rm=T)

annualR <- aggregate(Rain_data,format(time(Rain_data),"%Y"),sum, na.rm=T)
mean_annualR <- apply(annualR,2,mean,na.rm=T)
# Calculate RR
annual_RR <- annual/annualR
