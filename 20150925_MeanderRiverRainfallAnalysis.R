# Alternative rainfall stations for NIVE
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
library(xts)
library(zoo)
require(ggplot2)

start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")

closerainfall_stns <- data.frame("Meander" = "091061", "Meander2" = "091267")
for (i in seq_along(closerainfall_stns)) {
  temp<-read.csv(paste("IDCJAC0009_",closerainfall_stns[,i],"_1800_Data.csv", sep=""))
  temp$Date<-ISOdate(year=temp$Year, month=temp$Month, day=temp$Day)
  temp$Date<-as.Date(temp$Date)
  temp<-subset(temp, Date>=start_date & Date<=end_date, select=c(Date, Rainfall))
  assign(paste(colnames(closerainfall_stns[i]), "temp", sep=""), zoo(temp$Rainfall, order.by=temp$Date))
  print(i)
}
Rain_data<-merge(Meandertemp,Meander2temp,all=T)
# check missing data overlap
missing <- length(Rain_data$Meandertemp[is.na(Rain_data$Meandertemp)])
missing
missing <- ifelse(is.na(Rain_data$Meandertemp)==T,1,0)
missing2 <- ifelse(is.na(Rain_data$Meander2temp)==T,1,0)
plot(missing2)
points(missing,col="red")

sum(Rain_data$Meandertemp,na.rm=T)/40

# Patch Meander2 data into Meander data
Rain_data$Meandertemp <- Rain_data$Meandertemp + missing*Rain_data$Meandertemp2
missing <- length(Rain_data$Meandertemp[is.na(Rain_data$Meandertemp)])
missing
# does not make any difference

