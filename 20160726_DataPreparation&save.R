#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# Script to generate the required datasets
# Create Rdata and csv output that is transportable and exchangeable

# set working dir
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/ProjectData")

# packages to load
require(zoo)
require(xts)
# Define the date
Today <- format(Sys.Date(),"%Y%m%d")

# preliminaries
# define decades
study_period_decades <- c("70_80", "80_90", "90_00", "00_10")
decade_start <- c(as.Date("1/1/1970", format="%d/%m/%Y"), 
                  as.Date("1/1/1980", format="%d/%m/%Y"), 
                  as.Date("1/1/1990", format="%d/%m/%Y"), 
                  as.Date("1/1/2000", format="%d/%m/%Y"))
decade_end <- c(as.Date("31/12/1979", format="%d/%m/%Y"), 
                as.Date("31/12/1989", format="%d/%m/%Y"), 
                as.Date("31/12/1999", format="%d/%m/%Y"), 
                as.Date("31/12/2010", format="%d/%m/%Y"))

# define the overall period
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")

# subset function
subset_function <- function(x) {
  subset(x, as.Date(Date, format="%d/%m/%y")>=start_date & 
           as.Date(Date, format="%d/%m/%y")<=end_date)
}


### FLOW DATA ####

# Flow stations
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", 
                        "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", 
                        "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", 
                        "MURR"="405205", "SOUT"="225020A", "YARR"="614044", 
                        "DOMB"="607155")

# read in catchment characteristics to get sizes
CC <- read.csv("20150829_CatchmentCharact.csv")

# read un the flow data and convert to zoo
for (i in seq_along(flow_stns)) {
  temp <- read.csv(paste("Original streamflow data/",flow_stns[,i],"_daily_ts2.csv", sep=""), 
                   col.names=c("Date", "Q"))
  year <- substr(as.character(temp$Date),nchar(as.character(temp$Date))-1,nchar(as.character(temp$Date)))
  Dates <- as.Date(paste(substr(as.character(temp$Date),1,nchar(as.character(temp$Date))-2),
                         ifelse(as.numeric(year)>=50,paste("19",year,sep=""),paste("20",year,sep="")),sep=""),
                   "%d/%m/%Y")
  
  # convert ML/day to mm
  # 1 ML = 10^6 L = 10^6 dm^3 is 10^9 cm^3 is 10^12 mm^3
  # 1 km2 = 10^6 m^2 = 10^12 cm^2 = 10^14 mm^2
  # ML/day to mm --> flow/area(km2)/100 = mm
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""),
         zoo(temp$Q/(CC[i,4]),order.by=Dates))
}  
  #####

# merge all catchments to use same time interval
flow_zoo<-merge(COTT_daily_flow,RUTH_daily_flow,CORA_daily_flow,ELIZ_daily_flow,COCH_daily_flow,COEN_daily_flow,SCOT_daily_flow,
                HELL_daily_flow,NIVE_daily_flow,MURR_daily_flow,SOUT_daily_flow,YARR_daily_flow,DOMB_daily_flow)
flow_zoo <- window(flow_zoo, start=start_date, end=end_date)

# Also create a dataframe for flow
flow_data_70_10<-data.frame(Date=time(flow_zoo), coredata(flow_zoo))
colnames(flow_data_70_10)[2:14] <- colnames(flow_stns)
#####


# RAINFALL # #####
# Import data, create rainfall FDCs
closerainfall_stns <- data.frame("COTT"="070316", "RUTH"="069003", "CORA"="069049", 
                                 "ELIZ"="014149", "COCH"="031083", "COEN"="027005", 
                                 "SCOT"="023734", "HELL"="091109", "NIVE"="091065", 
                                 "MURR"="088028", "SOUT"="085238", "YARR"="009538", 
                                 "DOMB"="009590")
dates <- seq.Date(start_date, end_date, by="day")

# read in the data and subset to the required period
for (i in seq_along(flow_stns)) {
  temp<-read.csv(paste("Original Rainfall data/","IDCJAC0009_",closerainfall_stns[,i],
                       "_1800_Data.csv", sep=""))
  temp$Date<-ISOdate(year=temp$Year, month=temp$Month, day=temp$Day)
  temp$Date<-as.Date(temp$Date)
  temp<-subset(temp, Date>=start_date & Date<=end_date,
               select=c(9, 6))
  colnames(temp) <- c("Date", "Rainfall")
  assign(paste(colnames(flow_stns[i]), "temp", sep=""), 
         as.zoo(temp$Rainfall, order.by=temp$Date))
  print(i)
}
# merge 
rain_zoo<-merge(COTTtemp,RUTHtemp,CORAtemp,ELIZtemp,COCHtemp,COENtemp,SCOTtemp,
            HELLtemp,NIVEtemp,MURRtemp,SOUTtemp,YARRtemp,DOMBtemp)
rainfall_data_70_10<-data.frame(Date=time(rain_zoo), coredata(rain_zoo))
colnames(rainfall_data_70_10)[2:14] <- colnames(flow_stns)

# TEMPERATURE # #####
HQmaxT_stns <- data.frame("COTT"="070351", "RUTH"="070351", 
                          "CORA"="068072", "ELIZ"="014015", 
                          "COCH"="034084", "COEN"="027045", 
                          "SCOT"="023373", "HELL"="096003", 
                          "NIVE"="096003", "MURR"="085072", "SOUT"="085072", 
                          "YARR"="009021", "DOMB"="009518")
for (i in seq_along(flow_stns)) {
  temp <- read.csv(paste("Original Temperature data/",HQmaxT_stns[,i],".csv", sep=""))
  temp$Date <- as.Date(temp$Date, format="%d/%m/%Y")
  temp$maxT[temp$maxT==99999.9] <- NA
  temp<-subset(temp, Date>=start_date & Date<=end_date)
  assign(paste(colnames(flow_stns[i]), "temp.maxT", sep=""), 
         as.zoo(temp[,2], order.by=temp$Date))
  print(i)
}
# merge and create data.frame
maxT_zoo<-merge(COTTtemp.maxT,RUTHtemp.maxT,CORAtemp.maxT,
                ELIZtemp.maxT,COCHtemp.maxT,COENtemp.maxT,
                SCOTtemp.maxT,HELLtemp.maxT,NIVEtemp.maxT,
                MURRtemp.maxT,SOUTtemp.maxT,YARRtemp.maxT,
                DOMBtemp.maxT)

maxT_data_70_10<-data.frame(Date=time(maxT_zoo), coredata(maxT_zoo))

## WEEKLY MEANS ################
# calculate weekly values
for (i in 1:length(flow_stns)) {
  flow_t <- apply.weekly(flow_zoo[,i], sum)
  if (i ==1) flow_weekly <- flow_t else flow_weekly <- merge(flow_weekly,flow_t,all=T)
}
colnames(flow_weekly) <- colnames(flow_zoo)
for (i in 1:length(flow_stns)) {
  rain_t <- apply.weekly(rain_zoo[,i], sum)
  if (i ==1) rain_weekly <- rain_t else rain_weekly <- merge(rain_weekly,rain_t,all=T)
}
colnames(rain_weekly) <- colnames(rain_zoo)
for (i in 1:length(flow_stns)) {
  maxT_t <- apply.weekly(maxT_zoo[,i], mean)
  if (i ==1) maxT_weekly <- maxT_t else maxT_weekly <- merge(maxT_weekly,maxT_t,all=T)
}
colnames(maxT_weekly) <- colnames(maxT_zoo)


# ******************************************
# Stacking and merging into one dataset
# Now stack all the data together
# flow
flow_weekly_stack <- data.frame(Date=time(flow_weekly), coredata(flow_weekly))
colnames(flow_weekly_stack) <- c("Date", paste("flow",colnames(flow_stns),sep="."))

# add a column for the decade
for (j in 1:length(decade_start)) {
  flow_weekly_stack$decade[as.Date(flow_weekly_stack$Date) >= as.Date(decade_start[j]) &
                             as.Date(flow_weekly_stack$Date) <= as.Date(decade_end[j])] <- 
    study_period_decades[j]
}
# stack
flow_weekly_stack <- reshape(flow_weekly_stack, direction="long",
                             varying=2:14, sep=".")

# Now do the same for rainfall
rain_weekly_stack <- data.frame(Date=time(rain_weekly), coredata(rain_weekly))
colnames(rain_weekly_stack) <- c("Date", paste("rain",colnames(flow_stns),sep="."))

# add a column for the decade
for (j in 1:length(decade_start)) {
  rain_weekly_stack$decade[as.Date(rain_weekly_stack$Date) >= as.Date(decade_start[j]) &
                             as.Date(rain_weekly_stack$Date) <= as.Date(decade_end[j])] <- 
    study_period_decades[j]
}
# stack
rain_weekly_stack <- reshape(rain_weekly_stack, direction="long",
                             varying=2:14, sep=".")

# and for temperature
maxT_weekly_stack <- data.frame(Date=time(maxT_weekly), coredata(maxT_weekly))
colnames(maxT_weekly_stack) <- c("Date", paste("maxT",colnames(flow_stns),sep="."))

# add a column for the decade
for (j in 1:length(decade_start)) {
  maxT_weekly_stack$decade[as.Date(maxT_weekly_stack$Date) >= as.Date(decade_start[j]) &
                             as.Date(maxT_weekly_stack$Date) <= as.Date(decade_end[j])] <- 
    study_period_decades[j]
}
# stack
maxT_weekly_stack <- reshape(maxT_weekly_stack, direction="long",
                             varying=2:14, sep=".")


# Now merge all together into one dataset
flow_rain_maxT_weekly <- cbind(flow_weekly_stack[,1:4], rain_weekly_stack[,4],
                               maxT_weekly_stack[,4])
head(flow_rain_maxT_weekly)
colnames(flow_rain_maxT_weekly) <- c("Date","decade", "Station", "Flow", "Rain", "MaxT")

# write data frame as csv
write.csv(flow_rain_maxT_weekly,"flow_rain_maxT_weekly.csv")



# load metadata
zz <- file("README_DataDescription.txt","w+")
text <- c("Metadata Climate Change in Streamflow (MD project)",
"authors: R.Willem Vervoort, Michaela Dolk, Floris van Ogtrop",
"institution: Centre for Carbon Water and Food, Faculty of Agriculture and Environment, The University of Sydney",
"contact: willem.vervoort@sydney.edu.au",
"The data in this project are sourced from the Bureau of Meteorology website. ",
"They are based on a sample of the hydrological reference stations and closest rainfall and high quality temperature stations",
"These stations are given in 20150829_CatchmentCharact.csv and in the CC R object in the Rdata file",
"The objects in the Rdata file are:",
"This metadata file as a text object called README_datadescrib",
"CC a dataframe with the catchment characteristcs",
"flow_rain_maxT_weekly: a dataframe with column headers Date, decade, station, Flow, Rain, maxT, stations are stacked",
"flow_zoo: all flow data for the catchments as a zoo data frame, 13 catchments in columns",
"rain_zoo: all rain data for the catchments as a zoo data frame, 13 catchments in columns",
"maxT_zoo: all maxT data for the catchments as a zoo data frame, 13 catchments in columns",
"The objects in the zip file are:",
"This metadata file",
"20150829_CatchmentCharact.csv",
"flow_rain_maxT_weekly.csv: a dataframe with column headers Date, decade, station, Flow, Rain, maxT, stations are stacked")
writeLines(text,zz)
README_DataDescribe <- readLines(zz)
close(zz)

# write as an RData file
save("README_DataDescribe","flow_rain_maxT_weekly","CC","flow_zoo",
     "rain_zoo","maxT_zoo",file=paste(Today,"ClimCh_project_MD.Rdata",sep="_"))
# create also a zip file
zip(paste(Today,"ClimCh_project_MD.zip",sep="_"),
    c("README_DataDEscription.txt","flow_rain_maxT_weekly.csv",
      "20150829_CatchmentCharact.csv"))