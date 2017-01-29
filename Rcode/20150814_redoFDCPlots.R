####################################################
### CLIMATE CHANGE STREAM FLOW                    ##
### ~ Michaela Dolk and Willem Vervoort ~         ##
####################################################
# INPUT FILES:
# - Flow data: […]_daily_ts2.csv […] = flow station number
# - Rainfall data: IDCJAC0009_[…]_1800_Data.csv […] = rainfall station number
# - MaxT data: […].csv […] = HQmaxT station number
# - Flow station locations: Flow_station_locations.csv

# redo the FDC figures
Today <- format(Sys.Date(),"%Y%m%d")
# IMPORT FLOW DATA INTO R # #####
#setwd("/Users/michaeladolk/Desktop/Streamflow_data")
#setwd("U:/My-Workspace/mdol7996/Streamflow_2015")
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/streamflow_data")


# LOAD REQUIRED PACKAGES # #####
library(xts)
library(zoo)
library(Kendall)
library(mgcv)
library(oz)
#####
# STATIONS # #####
#flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
#####
# FLOW, RAINFALL AND TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
# Flow data
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""), read.csv(paste(flow_stns[,i],"_daily_ts2.csv", sep=""), col.names=c("Date", "Q")))
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
flow_stack <- flow_data_70_10
colnames(flow_stack) <- c("Date", "flow.COTT", "flow.RUTH", "flow.CORA", "flow.ELIZ", "flow.COCH", "flow.COEN", "flow.SCOT", "flow.HELL", "flow.NIVE", "flow.MURR", "flow.SOUT", "flow.YARR", "flow.DOMB")
flow_stack <- reshape(flow_stack, direction="long", varying=2:14, sep=".")
# Rainfall data
closerainfall_stns <- data.frame("COTT"="070316", "RUTH"="069003", "CORA"="069049", "ELIZ"="014149", "COCH"="031083", "COEN"="027005", "SCOT"="023734", "HELL"="091040", "NIVE"="096046", "MURR"="088028", "SOUT"="085238", "YARR"="009538", "DOMB"="009590")
for (i in seq_along(flow_stns)) {
  temp<-read.csv(paste("IDCJAC0009_",closerainfall_stns[,i],"_1800_Data.csv", sep=""))
  temp$Date<-ISOdate(year=temp$Year, month=temp$Month, day=temp$Day)
  temp$Date<-as.Date(temp$Date)
  temp<-subset(temp, Date>=start_date & Date<=end_date, select=c(Date, Rainfall))
  assign(paste(colnames(flow_stns[i]), "temp", sep=""), as.zoo(temp, order.by=temp$Date))
  print(i)
}
temp<-merge(COTTtemp,RUTHtemp,CORAtemp,ELIZtemp,COCHtemp,COENtemp,SCOTtemp,HELLtemp,NIVEtemp,MURRtemp,SOUTtemp,YARRtemp,DOMBtemp)
temp<-as.data.frame(temp)
rainfall_data_70_10<-data.frame(Date=row.names(temp), COTT=temp$Rainfall.COTTtemp,
                                RUTH=temp$Rainfall.RUTHtemp, CORA=temp$Rainfall.CORAtemp,
                                ELIZ=temp$Rainfall.ELIZtemp, COCH=temp$Rainfall.COCHtemp,
                                COEN=temp$Rainfall.COENtemp, SCOT=temp$Rainfall.SCOTtemp,
                                HELL=temp$Rainfall.HELLtemp, NIVE=temp$Rainfall.NIVEtemp,
                                MURR=temp$Rainfall.MURRtemp, SOUT=temp$Rainfall.SOUTtemp,
                                YARR=temp$Rainfall.YARRtemp, DOMB=temp$Rainfall.DOMBtemp)
# Temperature data
HQmaxT_stns <- data.frame("COTT"="070351", "RUTH"="070351", "CORA"="068072", "ELIZ"="014015", "COCH"="034084", "COEN"="027045", "SCOT"="023373", "HELL"="096003", "NIVE"="096003", "MURR"="085072", "SOUT"="085072", "YARR"="009021", "DOMB"="009518")
for (i in seq_along(flow_stns)) {
  temp <- read.csv(paste(HQmaxT_stns[,i],".csv", sep=""))
  temp$Date <- as.Date(temp$Date, format="%d/%m/%Y")
  temp$maxT[temp$maxT==99999.9] <- NA
  temp<-subset(temp, Date>=start_date & Date<=end_date)
  assign(paste(colnames(flow_stns[i]), "temp.maxT", sep=""), as.zoo(temp, order.by=temp$Date))
  print(i)
}
temp.maxT<-merge(COTTtemp.maxT,RUTHtemp.maxT,CORAtemp.maxT,ELIZtemp.maxT,COCHtemp.maxT,COENtemp.maxT,SCOTtemp.maxT,HELLtemp.maxT,NIVEtemp.maxT,MURRtemp.maxT,SOUTtemp.maxT,YARRtemp.maxT,DOMBtemp.maxT)
temp.maxT<-as.data.frame(temp.maxT)
maxT_data_70_10<-data.frame(Date=row.names(temp.maxT), COTT=temp.maxT$maxT.COTTtemp.maxT,
                            RUTH=temp.maxT$maxT.RUTHtemp.maxT, CORA=temp.maxT$maxT.CORAtemp.maxT,
                            ELIZ=temp.maxT$maxT.ELIZtemp.maxT, COCH=temp.maxT$maxT.COCHtemp.maxT,
                            COEN=temp.maxT$maxT.COENtemp.maxT, SCOT=temp.maxT$maxT.SCOTtemp.maxT,
                            HELL=temp.maxT$maxT.HELLtemp.maxT, NIVE=temp.maxT$maxT.NIVEtemp.maxT,
                            MURR=temp.maxT$maxT.MURRtemp.maxT, SOUT=temp.maxT$maxT.SOUTtemp.maxT,
                            YARR=temp.maxT$maxT.YARRtemp.maxT, DOMB=temp.maxT$maxT.DOMBtemp.maxT)
# Put in same data frame
dates_list <- rainfall_data_70_10$Date
dates_list <- as.Date(dates_list)
flow_xts <- xts(flow_data_70_10[2:14], order.by=dates_list, frequency=1)
flow_weekly_xts <- apply.weekly(flow_xts, mean)
weekly_flow_stack <- as.data.frame(flow_weekly_xts)
weekly_flow_stack <- data.frame(Date=row.names(weekly_flow_stack), weekly_flow_stack)
colnames(weekly_flow_stack) <- c("Date", "flow.COTT", "flow.RUTH", "flow.CORA", "flow.ELIZ", "flow.COCH", "flow.COEN", "flow.SCOT", "flow.HELL", "flow.NIVE", "flow.MURR", "flow.SOUT", "flow.YARR", "flow.DOMB")
weekly_flow_stack <- reshape(weekly_flow_stack, direction="long", varying=2:14, sep=".")
rainfall_xts <- xts(rainfall_data_70_10[2:14], order.by=dates_list, frequency=1)
rainfall_weekly_xts <- apply.weekly(rainfall_xts, mean)
weekly_rain_stack <- as.data.frame(rainfall_weekly_xts)
colnames(weekly_rain_stack) <- c("rain.COTT", "rain.RUTH", "rain.CORA", "rain.ELIZ", "rain.COCH", "rain.COEN", "rain.SCOT", "rain.HELL", "rain.NIVE", "rain.MURR", "rain.SOUT", "rain.YARR", "rain.DOMB")
weekly_rain_stack <- reshape(weekly_rain_stack, direction="long", varying=1:13, sep=".")
maxT_xts <- xts(maxT_data_70_10[2:14], order.by=dates_list, frequency=1)
maxT_weekly_xts <- apply.weekly(maxT_xts, mean)
weekly_maxT_stack <- as.data.frame(maxT_weekly_xts)
colnames(weekly_maxT_stack) <- c("maxT.COTT", "maxT.RUTH", "maxT.CORA", "maxT.ELIZ", "maxT.COCH", "maxT.COEN", "maxT.SCOT", "maxT.HELL", "maxT.NIVE", "maxT.MURR", "maxT.SOUT", "maxT.YARR", "maxT.DOMB")
weekly_maxT_stack <- reshape(weekly_maxT_stack, direction="long", varying=1:13, sep=".")
flow_rain_maxT_weekly <- data.frame(Date=weekly_flow_stack$Date, Station=weekly_flow_stack$time, Flow=weekly_flow_stack$flow, Rain=weekly_rain_stack$rain, MaxT=weekly_maxT_stack$maxT)
# Remove NAs
flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)
#####



#######################################################
##  ~ FDC curves ~  ##
#######################################################
# OUTPUT:
# - Flow FDC plots
# - Rainfall FDC plots
# FDC FLOW # #####
study_period_decades <- c("70_80", "80_90", "90_00", "00_10")
decade_start <- c(as.Date("1/1/1970", format="%d/%m/%Y"), as.Date("1/1/1980", format="%d/%m/%Y"), as.Date("1/1/1990", format="%d/%m/%Y"), as.Date("1/1/2000", format="%d/%m/%Y"))
decade_end <- c(as.Date("31/12/1979", format="%d/%m/%Y"), as.Date("31/12/1989", format="%d/%m/%Y"), as.Date("31/12/1999", format="%d/%m/%Y"), as.Date("31/12/2009", format="%d/%m/%Y"))
###
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), ".flow.x.70_80", sep=""), 
         subset(flow_rain_maxT_weekly, 
                Station==colnames(flow_stns[i]) & as.Date(Date)>=as.Date("1/1/1970", format="%d/%m/%Y") & as.Date(Date)<=as.Date("1/1/1980", format="%d/%m/%Y"), 
                select=Flow))
  assign(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep=""), 
         quantile(get(paste(colnames(flow_stns[i]), ".flow.x.70_80", sep=""))$Flow, 
                  probs=seq(0,1,length=100)))
  for (j in seq_along(study_period_decades[2:4])) {
    assign(paste(colnames(flow_stns[i]), ".flow.x.", study_period_decades[2:4][j], sep=""), subset(flow_rain_maxT_weekly, Station==colnames(flow_stns[i]) & as.Date(Date)>=decade_start[j+1] & as.Date(Date)<=decade_end[j+1], select=Flow))
    assign(paste(colnames(flow_stns[i]), ".flow.y.", study_period_decades[2:4][j], sep=""), quantile(get(paste(colnames(flow_stns[i]), ".flow.x.", study_period_decades[2:4][j], sep=""))$Flow, probs=seq(0,1,length=100)))
  }
  print(i)
}
colour_list <- c("red", "purple", "blue", "green")
# DIFFERENCE BETWEEN WEEKLY FDCs DIVIDED BY 1970-1979 FDCs -> not log, ylim=c(-1,1) to prevent issues plotting when denominator 0 (gives -Inf) hence not all values shown
require(ggplot2)

plot.list <- vector("list", length=13)
for (i in seq_along(flow_stns)) {
  temp1 <- data.frame(prob=1-seq(0,1,length=100), 
                        diff = (get(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep=""))-
                            get(paste(colnames(flow_stns[i]), ".flow.y.80_90", sep="")))/
                            get(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep="")),
                        period = "((1970-1979) - (1980-1989))/(1970-1979)",
                        station=names(flow_stns[i]))
  temp2 <- data.frame(prob=1-seq(0,1,length=100), 
                      diff = (get(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep=""))-
                        get(paste(colnames(flow_stns[i]), ".flow.y.90_00", sep="")))/
                        get(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep="")),
                      period = "((1970-1979) - (1990-1999))/(1970-1979)",
                      station=names(flow_stns[i]))
  temp3 <- data.frame(prob=1-seq(0,1,length=100), 
                      diff = (get(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep=""))-
                                get(paste(colnames(flow_stns[i]), ".flow.y.00_10", sep="")))/
                        get(paste(colnames(flow_stns[i]), ".flow.y.70_80", sep="")),
                      period = "((1970-1979) - (2000-2009))/(1970-1979)",
                      station=names(flow_stns[i]))
  plot.list[[i]] <- rbind(temp1,temp2,temp3)
}

plot.df <- do.call(rbind,plot.list)

#windows(width=20,height=15)
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_StreamflowFDCDifference.tif",sep=""),width=960,height=720)
p <- ggplot(plot.df, aes(x = prob, y = diff)) +
  geom_line(aes(linetype=period, colour=period),size=1.2) + 
  facet_wrap(~ station,ncol=5) + ylim(c(-2,2)) +
  theme(legend.position="bottom")
p + ggtitle("Streamflow") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Probability") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Scaled Difference") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
 scale_colour_manual(values=c("red", "blue", "green")) +
  theme(legend.text = element_text( size = 14))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))
dev.off()


#####
# CDF RAINFALL # #####
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), ".rain.x.70_80", sep=""), subset(flow_rain_maxT_weekly.na_removed, Station==colnames(flow_stns[i]) & as.Date(Date)>=as.Date("1/1/1970", format="%d/%m/%Y") & as.Date(Date)<=as.Date("1/1/1980", format="%d/%m/%Y"), select=Rain))
  assign(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep=""), quantile(get(paste(colnames(flow_stns[i]), ".rain.x.70_80", sep=""))$Rain, probs=seq(0,1,length=100)))
  for (j in seq_along(study_period_decades[2:4])) {
    assign(paste(colnames(flow_stns[i]), ".rain.x.", study_period_decades[2:4][j],
                 sep=""), subset(flow_rain_maxT_weekly.na_removed, Station==colnames(flow_stns[i]) & as.Date(Date)>=decade_start[j+1] & as.Date(Date)<=decade_end[j+1], select=Rain))
    assign(paste(colnames(flow_stns[i]), ".rain.y.", study_period_decades[2:4][j], sep=""), 
           quantile(get(paste(colnames(flow_stns[i]), ".rain.x.", study_period_decades[2:4][j], sep=""))$Rain, probs=seq(0,1,length=100)))
  }
  print(i)
}
# DIFFERENCE BETWEEN WEEKLY FDCs DIVIDED BY 1970-1979 FDCs -> not log, ylim=c(-1,1) to prevent issues plotting when denominator 0 (gives -Inf) hence not all values shown
plot.list <- vector("list", length=13)
for (i in seq_along(flow_stns)) {
  temp1 <- data.frame(prob=1-seq(0,1,length=100), 
                      diff = (get(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep=""))-
                                get(paste(colnames(flow_stns[i]), ".rain.y.80_90", sep="")))/
                        get(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep="")),
                      period = "((1970-1979) - (1980-1989))/(1970-1979)",
                      station=names(flow_stns[i]))
  temp2 <- data.frame(prob=1-seq(0,1,length=100), 
                      diff = (get(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep=""))-
                                get(paste(colnames(flow_stns[i]), ".rain.y.90_00", sep="")))/
                        get(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep="")),
                      period = "((1970-1979) - (1990-1999))/(1970-1979)",
                      station=names(flow_stns[i]))
  temp3 <- data.frame(prob=1-seq(0,1,length=100), 
                      diff = (get(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep=""))-
                                get(paste(colnames(flow_stns[i]), ".rain.y.00_10", sep="")))/
                        get(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep="")),
                      period = "((1970-1979) - (2000-2009))/(1970-1979)",
                      station=names(flow_stns[i]))
  plot.list[[i]] <- rbind(temp1,temp2,temp3)
}

plot.df <- do.call(rbind,plot.list)

windows(width=20,height=15)
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_RainfallFDCDifference.tif",sep=""),width=960,height=720)
p <- ggplot(plot.df, aes(x = prob, y = diff)) +
  geom_line(aes(linetype=period, colour=period),size=1.2) + 
  facet_wrap(~ station,ncol=5) + ylim(c(-2,2)) +
  theme(legend.position="bottom")
p + ggtitle("Rainfall") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Probability") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Scaled Difference") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
  scale_colour_manual(values=c("red", "blue", "green")) +
  theme(legend.text = element_text( size = 14))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))
dev.off()