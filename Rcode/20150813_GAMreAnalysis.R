#################################
### CLIMATE CHANGE STREAM FLOW ##
###         ~ Week 3 ~         ##
#################################

# IMPORT FLOW DATA INTO R # #####
#setwd("/Users/michaeladolk/Desktop/Streamflow_data")
#setwd("U:/My-Workspace/mdol7996/Streamflow_2015")
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/streamflow_data")

require(zoo)
require(xts)
Today <- format(Sys.Date(),"%Y%m%d")

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
                                ELIZ=temp$Rainfall.ELIZtemp, COCH=temp$Rainfall.COCHtemp,
                                COEN=temp$Rainfall.COENtemp, SCOT=temp$Rainfall.SCOTtemp,
                                HELL=temp$Rainfall.HELLtemp, NIVE=temp$Rainfall.NIVEtemp,
                                MURR=temp$Rainfall.MURRtemp, SOUT=temp$Rainfall.SOUTtemp,
                                YARR=temp$Rainfall.YARRtemp, DOMB=temp$Rainfall.DOMBtemp)
for (i in seq_along(study_period_decades)) {assign(paste("rainfall.", study_period_decades[i], sep=""), subset(rainfall_data_70_10, as.Date(Date)>=decade_start[i] & as.Date(Date)<=decade_end[i]))}
#rain_stack <- rainfall_data_70_10
#colnames(rain_stack) <- c("Date", "rain.COTT", "rain.RUTH", "rain.CORA", "rain.ELIZ", "rain.COCH", "rain.COEN", "rain.SCOT", "rain.HELL", "rain.NIVE", "rain.MURR", "rain.SOUT", "rain.YARR", "rain.DOMB")
#rain_stack <- reshape(rain_stack, direction="long", varying=2:14, sep=".")
#flow_rain <- data.frame(Date=flow_stack$Date, Station=flow_stack$time, Flow=flow_stack$flow, Rain=rain_stack$rain)
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
                            ELIZ=temp.maxT$maxT.ELIZtemp.maxT, COCH=temp.maxT$maxT.COCHtemp.maxT,
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

## GAMM analysis
library(mgcv)
library(ggplot2)
# we cannot use gamm4 as this will not allow correlation structures and no te()


#with corr
flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(flow_rain_maxT_weekly, Station==colnames(flow_stns[i]))
#  gamm.data$lagRain <- lag(gamm.data$Rain)
#  gamm.data <- na.omit(gamm.data)
#  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gamm(log(Flow+1)~s(Rain) + s(Rain,MaxT), 
        corr = corCAR1(), data=gamm.data, control=list(niterEM=0)))
  print(i)
}

gamm.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list[[i]] <- get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))
}
save(gamm.list,file=paste(Today,"gamm.list_noLag.Rdata",sep="_"))

load("20150811_gamm.list_noLag.Rdata")

tiff(paste(Today,"gamm_residuals_noLag.tif"),width=480,height=720)
par(mfrow=c(5,3))
for (i in seq_along(flow_stns)) {
  plot(residuals(get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))$lme,
                 type="normalized"), main=colnames(flow_stns[i]),
       ylab="normalised residuals")
  n <- length(residuals(get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))$lme,type="normalized"))
  abline(lsfit(1:n, residuals(get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))$lme,type="normalized")), col="red")
}
dev.off()


# do mann kendall on the residuals
require(Kendall)
require(ggplot2)
resid.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  resid.list[[i]] <- zoo(residuals(gamm.list[[i]]$lme,
                 type="normalized"),
                 order.by=as.Date(na.omit(subset(flow_rain_maxT_weekly, 
                                                 Station==colnames(flow_stns[i])))$Date))
}
resid.df <- do.call(merge.zoo,resid.list)
names(resid.df) <- colnames(flow_stns)
# Bootstrap
set.seed(10)
# now run a loop over the number of years (create 41 different sets)
# do Mann Kendall test on each resonstituted series
# ---------------------------
#  -------------------------
resid.temp <- as.data.frame(resid.df)
resid.temp$years <- format(time(resid.df),"%Y")
split.resid <- split(resid.temp[,1:13],resid.temp$years)

MK.list <- list()

for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(resid.df),nyears(resid.df))
  for (j in 1:length(series)) {
    #j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.resid[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.resid[[series[j]]]))
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
  sig.set[[i]] <- set[set$pvalue < 0.5,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))


MK.resid <- do.call(rbind,lapply(resid.list,MannKendall))


real.df <- data.frame(pvalue = as.numeric(MK.resid[,2]), 
                      tau = as.numeric(MK.resid[,1]),
                      catch=colnames(flow_stns),
                      type=rep("real",nrow(MK.resid)))
# A histogram of taus
tiff(paste(Today,"_FigureResidGAM_MDPaper.tif",sep=""),res=600,compression="lzw",
     width=10*480,height=10*480)
hp <- ggplot(sig.set.a, aes(x=tau)) + geom_histogram(binwidth=0.03,colour="white")
#print(hp)
# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  scale_colour_brewer(palette="Set1") +
  facet_wrap(~ catch,ncol=5)+ ggtitle("Residuals Streamflow after GAM") #+
dev.off()
# -------- end flow -----------------


#----------------------------------
# INSERT TREND IN MODEL
# --------------------------------------

#with corr and trend
flow_rain_maxT_weekly.na_removed <- na.omit(flow_rain_maxT_weekly)
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(flow_rain_maxT_weekly, Station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  #  gamm.data$lagRain <- lag(gamm.data$Rain)
  #  gamm.data <- na.omit(gamm.data)
  #  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gamm(log(Flow+1)~ trend + s(Rain) + s(Rain,MaxT), 
              corr = corCAR1(), data=gamm.data, control=list(niterEM=0)))
  print(i)
}
# Put the gamm results in a list and store
gamm.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list[[i]] <- get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))
}
save(gamm.list,file=paste(Today,"gamm.list_noLagTrend.Rdata",sep="_"))


# read in the GAMM results to look at rsq adj
load("20150814_gamm.list_noLagTrend.Rdata")
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  print(summary(gamm.list[[i]]$gam)$p.table[,4])
  print(summary(gamm.list[[i]]$gam)$s.table[,4])
  print(summary(gamm.list[[i]]$gam)$r.sq)
}

# create data frames for the replotting of the interaction plots
plot.l2 <- list()
# extract the plot data
for (i in seq_along(flow_stns)) {
  test <- plot.gam(gamm.list[[i]]$gam, pages =1, select=2)
  plot.l2[[i]] <- data.frame(fit=test[[2]]$fit, x=rep(test[[2]]$x,40),
                        y = sort(rep(test[[2]]$y,40)), 
                        station = names(flow_stns)[i])
}  
plot.df2 <- do.call(rbind,plot.l2)
require(lattice)

# my.strip2 <- function(which.given, ..., factor.levels, bg, par.strip.text) {
#   levs <- if (which.given == 1) c("Oenpelli","Tennant Creek") 
#   else c("changed s*","not changed s*")
#   my.bg <- "gray90"
#   strip.default(which.given, ..., factor.levels = levs, 
#                 bg = my.bg, par.strip.text=list(font=2,cex=1.2))
# }


# Make a nice levelplot for the interactions
tiff(paste(Today,"_TrendsMaxTRainGamm.tif",sep=""),res=600,compression="lzw",
     width=10*480,height=10*480)
trellis.par.set("regions", list(col=heat.colors(100)))
trellis.par.set("background", list(col="white"))
levelplot(fit~x+y|station, data=plot.df2, scales="free",
          xlab=list("Weekly rainfall (mm)",font=2,cex=1.2),
          ylab=list("Weekly maximum T (C)",font=2,cex=1.2),
          contour=T)
dev.off()



# USe ggplot to plot the smooths
# This is some code I found somewhere, but had to tweak
# create data frames for the replotting of the interaction plots
plot.l1 <- list()
points.l1 <- list()
# extract the plot data
for (i in seq_along(flow_stns)) {
  png(paste0(tempdir(),"/temporary_R_file.png"))
    plot.temp <- plot.gam(gamm.list[[i]]$gam, residuals=T,pages =1, select=1)
  dev.off()
    
  plot.l1[[i]] <- data.frame(x=plot.temp[[1]]$x, smooth=plot.temp[[1]]$fit, 
                             se=plot.temp[[1]]$se, 
                             station = names(flow_stns)[i])
  points.l1[[i]] <- data.frame(data.x=plot.temp[[1]]$raw, data.y=plot.temp[[1]]$p.resid, 
          station = names(flow_stns)[i])

}  
plot.df1 <- do.call(rbind,plot.l1)
points.df1 <- do.call(rbind,points.l1)

# make a fancy plot of the smooths
tiff(paste(Today,"_TrendsRainGamm.tif",sep=""),res=600,compression="lzw",
     width=10*480,height=10*480)

    ggplot(plot.df1, aes(x, smooth)) + geom_line(col="black", size=1) +
    geom_point(data = points.df1, aes(data.x, data.y), col="gray50", size=1, alpha=I(0.2)) +    
    geom_line(data=plot.df1,aes(y=smooth-se), linetype="dashed", col="red", size=1) + 
    geom_line(data=plot.df1,aes(y=smooth+se), linetype="dashed", col="red", size=1) + 
      facet_wrap(~station,scales="free") + theme_bw() + xlab("Rainfall (mm)") +
      ylab("Response relative to mean") +
      theme(axis.title.x = element_text(face="bold",  size=12), 
            axis.title.y = element_text(face="bold",  size=12))
dev.off()
      


