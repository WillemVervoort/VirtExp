####################################################
### CLIMATE CHANGE STREAM FLOW                    ##
### ~ Michaela Dolk and Willem Vervoort ~         ##
####################################################
# INPUT FILES:
# - Flow data: […]_daily_ts2.csv […] = flow station number
# - Rainfall data: IDCJAC0009_[…]_1800_Data.csv […] = rainfall station number
# - MaxT data: […].csv […] = HQmaxT station number
# - Flow station locations: Flow_station_locations.csv

# This version (past 20150830) flow data converted from ML/day to mm

# redo the FDC figures
Today <- format(Sys.Date(),"%Y%m%d")
# IMPORT FLOW DATA INTO R # #####
#setwd("/Users/michaeladolk/Desktop/Streamflow_data")
#setwd("U:/My-Workspace/mdol7996/Streamflow_2015")
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")


# LOAD REQUIRED PACKAGES # #####
library(xts)
library(zoo)
library(Kendall)
library(mgcv)
library(oz)
#####
# STATIONS # #####
#####
# FLOW, RAINFALL AND TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
# Flow data
# read in catchment characteristics to get sizes

load("ProjectData/20160804_ClimCh_project_MD.Rdata")




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
p <- ggplot(plot.df, aes(x = prob, y = diff)) +
  geom_line(aes(linetype=period, colour=period),size=1.2) + 
  facet_wrap(~ station,ncol=5) + ylim(c(-2,2)) +
  theme(legend.position="bottom") +
  guides(col = guide_legend(nrow = 3))
p <- p + ggtitle("Streamflow") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Probability") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Scaled Difference") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
 scale_colour_manual(values=c("black", "gray33", "gray66")) +
  theme(legend.text = element_text( size = 12))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))

save(p,file=paste(Today,"_StreamflowFDC.RData"))

# Normal quality
tiff(paste("manuscript drafts/",
           Today,"_StreamflowFDCDifference.tif",sep=""),width=720,height=720)
print(p)
dev.off()



# publication quality
tiff(paste("manuscript/",Today,"Figure6_StreamflowFDCDifference.tif",sep=""),width=16*480,height=12*480,
     res=600, compression="lzw")
print(p)
dev.off()


#####
# CDF RAINFALL # #####
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), ".rain.x.70_80", sep=""), 
         subset(flow_rain_maxT_weekly, 
        Station==colnames(flow_stns[i]) & as.Date(Date)>=as.Date("1/1/1970",
        format="%d/%m/%Y") & as.Date(Date)<=as.Date("1/1/1980", format="%d/%m/%Y"), 
        select=Rain))
  assign(paste(colnames(flow_stns[i]), ".rain.y.70_80", sep=""), 
         quantile(get(paste(colnames(flow_stns[i]), 
              ".rain.x.70_80", sep=""))$Rain, probs=seq(0,1,length=100),na.rm=T))
  for (j in seq_along(study_period_decades[2:4])) {
    assign(paste(colnames(flow_stns[i]), ".rain.x.", study_period_decades[2:4][j], sep=""), 
          subset(flow_rain_maxT_weekly, 
          Station==colnames(flow_stns[i]) & as.Date(Date)>=decade_start[j+1] & 
          as.Date(Date)<=decade_end[j+1], select=Rain))
    assign(paste(colnames(flow_stns[i]), ".rain.y.", study_period_decades[2:4][j], sep=""), 
           quantile(get(paste(colnames(flow_stns[i]), ".rain.x.", 
          study_period_decades[2:4][j], sep=""))$Rain, probs=seq(0,1,length=100),
          na.rm=T))
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

p <- ggplot(plot.df, aes(x = prob, y = diff)) +
  geom_line(aes(linetype=period, colour=period),size=1.2) + 
  facet_wrap(~ station,ncol=5) + ylim(c(-2,2)) +
  theme(legend.position="bottom") +
  guides(col = guide_legend(nrow = 3))
p <- p + ggtitle("Rainfall") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Probability") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Scaled Difference") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
  scale_colour_manual(values=c("black", "gray33", "gray66")) +
  theme(legend.text = element_text( size = 14))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))
save(p,file=paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
                  Today,"_RainfallFDCDifference,Rdata",sep=""))

#windows(width=20,height=15)
# draft quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_RainfallFDCDifference.tif",sep=""),width=960,height=720)
print(p)
dev.off()

# publication quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_RainfallFDCDifference.tif",sep=""),width=16*480,height=12*480,
     res=600, compression="lzw")
print(p)
dev.off()

