#################################
### CLIMATE CHANGE STREAM FLOW ##
###         ~ Week 3 ~         ##
#################################

# IMPORT FLOW DATA INTO R # #####
#setwd("/Users/michaeladolk/Desktop/Streamflow_data")
#setwd("U:/My-Workspace/mdol7996/Streamflow_2015")
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/streamflow_data")

require(zoo)

flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""), read.csv(paste(flow_stns[,i],"_daily_ts2.csv", sep=""), col.names=c("Date", "Q")))
}
#####

# SELECT STUDY PERIOD # #####
# Note: could possibly merge this section with next section
flow_df_list <- list(COTT=COTT_daily_flow, RUTH=RUTH_daily_flow, CORA=CORA_daily_flow, ELIZ=ELIZ_daily_flow, COCH=COCH_daily_flow, COEN=COEN_daily_flow, SCOT=SCOT_daily_flow, HELL=HELL_daily_flow, NIVE=NIVE_daily_flow, MURR=MURR_daily_flow, SOUT=SOUT_daily_flow, YARR=YARR_daily_flow, DOMB=DOMB_daily_flow)
study_period_decades <- c("70_80", "80_90", "90_00", "00_10")
decade_start <- c(as.Date("1/1/1970", format="%d/%m/%Y"), as.Date("1/1/1980", format="%d/%m/%Y"), as.Date("1/1/1990", format="%d/%m/%Y"), as.Date("1/1/2000", format="%d/%m/%Y"))
decade_end <- c(as.Date("31/12/1979", format="%d/%m/%Y"), as.Date("31/12/1989", format="%d/%m/%Y"), as.Date("31/12/1999", format="%d/%m/%Y"), as.Date("31/12/2009", format="%d/%m/%Y"))
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
#####


# RAINFALL # #####
# Import data, create rainfall FDCs
closerainfall_stns <- data.frame("COTT"="070316", "RUTH"="069003", "CORA"="069049", "ELIZ"="014149", "COCH"="031083", "COEN"="027005", "SCOT"="023734", "HELL"="091040", "NIVE"="096046", "MURR"="088028", "SOUT"="085238", "YARR"="009538", "DOMB"="009590")
dates <- seq.Date(start_date, end_date, by="day")
par(mfrow = c(3,5))
for (i in seq_along(flow_stns)) {
  temp<-read.csv(paste("IDCJAC0009_",closerainfall_stns[,i],"_1800_Data.csv", sep=""))
  temp$Date<-ISOdate(year=temp$Year, month=temp$Month, day=temp$Day)
  temp$Date<-as.Date(temp$Date)
  temp<-subset(temp, Date>=start_date & Date<=end_date, select=c(Date, Rainfall))
}
# STACK FLOW AND RAINFALL DATA #
flow_stack <- flow_data_70_10
colnames(flow_stack) <- c("Date", "flow.COTT", "flow.RUTH", "flow.CORA", "flow.ELIZ", "flow.COCH", "flow.COEN", "flow.SCOT", "flow.HELL", "flow.NIVE", "flow.MURR", "flow.SOUT", "flow.YARR", "flow.DOMB")
flow_stack <- reshape(flow_stack, direction="long", varying=2:14, sep=".")
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
                                ELIZ=temp$Rainfall.RUTHtemp, COCH=temp$Rainfall.CORAtemp,
                                COEN=temp$Rainfall.COENtemp, SCOT=temp$Rainfall.SCOTtemp,
                                HELL=temp$Rainfall.HELLtemp, NIVE=temp$Rainfall.NIVEtemp,
                                MURR=temp$Rainfall.MURRtemp, SOUT=temp$Rainfall.SOUTtemp,
                                YARR=temp$Rainfall.YARRtemp, DOMB=temp$Rainfall.DOMBtemp)
for (i in seq_along(study_period_decades)) {assign(paste("rainfall.", study_period_decades[i], sep=""), subset(rainfall_data_70_10, as.Date(Date)>=decade_start[i] & as.Date(Date)<=decade_end[i]))}
#rain_stack <- rainfall_data_70_10
#colnames(rain_stack) <- c("Date", "rain.COTT", "rain.RUTH", "rain.CORA", "rain.ELIZ", "rain.COCH", "rain.COEN", "rain.SCOT", "rain.HELL", "rain.NIVE", "rain.MURR", "rain.SOUT", "rain.YARR", "rain.DOMB")
#rain_stack <- reshape(rain_stack, direction="long", varying=2:14, sep=".")
#flow_rain <- data.frame(Date=flow_stack$Date, Station=flow_stack$time, Flow=flow_stack$flow, Rain=rain_stack$rain)
require(xts)

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
flow_rain_weekly <- data.frame(Date=weekly_flow_stack$Date, Station=weekly_flow_stack$time, Flow=weekly_flow_stack$flow, Rain=weekly_rain_stack$rain)

# REGRESSION
library(mgcv)

flow_rain_weekly.na_removed <- na.omit(flow_rain_weekly)
for (i in seq_along(flow_stns)) {
  assign(paste("flow_rain_weekly.", colnames(flow_stns[i]), ".na_removed", sep=""), subset(flow_rain_weekly.na_removed, Station==colnames(flow_stns[i])))
  assign(paste("flow_weekly_gam.", colnames(flow_stns[i]), sep=""), gamm(log(Flow+1)~s(Rain), corr = corCAR1(), data=get(paste("flow_rain_weekly.", colnames(flow_stns[i]), ".na_removed", sep=""))))
  print(i)
}

par(mfrow=c(3,5))
for (i in seq_along(flow_stns)) {
  plot(residuals(get(paste("flow_weekly_gam.", colnames(flow_stns[i]), sep=""))$gam), main=colnames(flow_stns[i]))
  n <- length(residuals(get(paste("flow_weekly_gam.", colnames(flow_stns[i]), sep=""))$gam))
  abline(lsfit(1:n, residuals(get(paste("flow_weekly_gam.", colnames(flow_stns[i]), sep=""))$gam)), col="red")
}

par(mfrow=c(3,5))
for (i in seq_along(flow_stns)) {
  plot(get(paste("flow_weekly_gam.", colnames(flow_stns[i]), sep=""))$gam, main=colnames(flow_stns[i]))
}

#Saving test
save(flow_weekly_gam.COTT, file="test_model.rda")
save(flow_weekly_gam.COTT, file="flow_weekly_gam.COTT.rda")
saveRDS(flow_weekly_gam.COTT, file="test_model2.rds")

#Save models
saveRDS(flow_weekly_gam.COTT, file="model_COTT.rds")
saveRDS(flow_weekly_gam.RUTH, file="model_RUTH.rds")
saveRDS(flow_weekly_gam.CORA, file="model_CORA.rds")
saveRDS(flow_weekly_gam.ELIZ, file="model_ELIZ.rds")
saveRDS(flow_weekly_gam.COCH, file="model_COCH.rds")
saveRDS(flow_weekly_gam.COEN, file="model_COEN.rds")
saveRDS(flow_weekly_gam.SCOT, file="model_SCOT.rds")
saveRDS(flow_weekly_gam.HELL, file="model_HELL.rds")
saveRDS(flow_weekly_gam.NIVE, file="model_NIVE.rds")
saveRDS(flow_weekly_gam.MURR, file="model_MURR.rds")
saveRDS(flow_weekly_gam.SOUT, file="model_SOUT.rds")
saveRDS(flow_weekly_gam.YARR, file="model_YARR.rds")
saveRDS(flow_weekly_gam.DOMB, file="model_DOMB.rds")

#save residuals
saveRDS(residuals(flow_weekly_gam.COTT$gam), file="residuals_COTT.rds")
saveRDS(residuals(flow_weekly_gam.RUTH$gam), file="residuals_RUTH.rds")
saveRDS(residuals(flow_weekly_gam.CORA$gam), file="residuals_CORA.rds")
saveRDS(residuals(flow_weekly_gam.ELIZ$gam), file="residuals_ELIZ.rds")
saveRDS(residuals(flow_weekly_gam.COCH$gam), file="residuals_COCH.rds")
saveRDS(residuals(flow_weekly_gam.COEN$gam), file="residuals_COEN.rds")
saveRDS(residuals(flow_weekly_gam.SCOT$gam), file="residuals_SCOT.rds")
saveRDS(residuals(flow_weekly_gam.HELL$gam), file="residuals_HELL.rds")
saveRDS(residuals(flow_weekly_gam.NIVE$gam), file="residuals_NIVE.rds")
saveRDS(residuals(flow_weekly_gam.MURR$gam), file="residuals_MURR.rds")
saveRDS(residuals(flow_weekly_gam.SOUT$gam), file="residuals_SOUT.rds")
saveRDS(residuals(flow_weekly_gam.YARR$gam), file="residuals_YARR.rds")
saveRDS(residuals(flow_weekly_gam.DOMB$gam), file="residuals_DOMB.rds")

# TEMPERATURE # #####
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
                                ELIZ=temp.maxT$maxT.RUTHtemp.maxT, COCH=temp.maxT$maxT.CORAtemp.maxT,
                                COEN=temp.maxT$maxT.COENtemp.maxT, SCOT=temp.maxT$maxT.SCOTtemp.maxT,
                                HELL=temp.maxT$maxT.HELLtemp.maxT, NIVE=temp.maxT$maxT.NIVEtemp.maxT,
                                MURR=temp.maxT$maxT.MURRtemp.maxT, SOUT=temp.maxT$maxT.SOUTtemp.maxT,
                                YARR=temp.maxT$maxT.YARRtemp.maxT, DOMB=temp.maxT$maxT.DOMBtemp.maxT)
maxT_xts <- xts(maxT_data_70_10[2:14], order.by=dates_list, frequency=1)
maxT_weekly_xts <- apply.weekly(maxT_xts, mean)
weekly_maxT_stack <- as.data.frame(maxT_weekly_xts)
colnames(weekly_maxT_stack) <- c("maxT.COTT", "maxT.RUTH", "maxT.CORA", "maxT.ELIZ", "maxT.COCH", "maxT.COEN", "maxT.SCOT", "maxT.HELL", "maxT.NIVE", "maxT.MURR", "maxT.SOUT", "maxT.YARR", "maxT.DOMB")
weekly_maxT_stack <- reshape(weekly_maxT_stack, direction="long", varying=1:13, sep=".")
flow_rain_maxT_weekly <- data.frame(Date=weekly_flow_stack$Date, Station=weekly_flow_stack$time, Flow=weekly_flow_stack$flow, Rain=weekly_rain_stack$rain, MaxT=weekly_maxT_stack$maxT)
head(flow_rain_maxT_weekly)
#####

# REGRESSION TEMPERATURE -> need to check corr
library(mgcv)

#test
flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)
test <- subset(flow_rain_maxT_weekly.na_removed, Station=="COTT")
test.gam <- gam(log(Flow+1)~s(MaxT), data=test)
pacf(residuals(test.gam))

#with corr
flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)
for (i in seq_along(flow_stns)) {
  assign(paste("flow_rain_maxT_weekly.", colnames(flow_stns[i]), ".na_removed", sep=""), subset(flow_rain_maxT_weekly.na_removed, Station==colnames(flow_stns[i])))
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), gamm(log(Flow+1)~s(MaxT), corr = corCAR1(), data=get(paste("flow_rain_maxT_weekly.", colnames(flow_stns[i]), ".na_removed", sep=""))))
  print(i)
}

par(mfrow=c(3,5))
for (i in seq_along(flow_stns)) {
  plot(residuals(get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))$gam), main=colnames(flow_stns[i]))
  n <- length(residuals(get(paste("flow_weekly_gam.temp", colnames(flow_stns[i]), sep=""))$gam))
  abline(lsfit(1:n, residuals(get(paste("flow_weekly_gam.temp", colnames(flow_stns[i]), sep=""))$gam)), col="red")
}

par(mfrow=c(3,5))
for (i in seq_along(flow_stns)) {
  plot(get(paste("flow_weekly_gam.temp", colnames(flow_stns[i]), sep=""))$gam, main=colnames(flow_stns[i]))
}



