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


### FLOW DATA ####
# read in the non-gridded data
load("20160726_ClimCh_project_MD.Rdata")

# RAINFALL # #####
#read in the gridded rainfall data
load("GriddedRainfallData.Rdata")

# calculate weekly sums of rainfall
ttime <- time(output.z[2:nrow(output.z),])
gridRainWeekly <- apply(output.z[2:nrow(output.z),], 2,
                        function(x) apply.weekly(xts(x,order.by=ttime),sum))
temp <- apply.weekly(output.z[2:nrow(output.z),],sum)
gridRainWeekly.z <- zoo(gridRainWeekly,
                        order.by=time(temp))
colnames(gridRainWeekly.z) <- CC[,1]
head(gridRainWeekly.z)


# ******************************************
# Stacking and merging into one dataset
# Now stack all the data together
gridRain_weekly_stack <- data.frame(Date=time(gridRainWeekly.z), 
                                    coredata(gridRainWeekly.z))
colnames(gridRain_weekly_stack) <- c("Date", paste("gridRain",
                                          CC[,1],sep="."))

# add a column for the decade
for (j in 1:length(decade_start)) {
  gridRain_weekly_stack$decade[as.Date(gridRain_weekly_stack$Date) >= as.Date(decade_start[j]) &
                             as.Date(gridRain_weekly_stack$Date) <= as.Date(decade_end[j])] <- 
    study_period_decades[j]
}
# stack
gridRain_weekly_stack <- reshape(gridRain_weekly_stack, direction="long",
                             varying=2:14, sep=".")

# add to flow_rain_maxT_weekly
GridRainAllDataout <- cbind(flow_rain_maxT_weekly,gridRain_weekly_stack[,4])
colnames(GridRainAllDataout)[7] <- "gridRain"
# write data frame as Rdata and csv
save(GridRainAllDataout,
     file=paste(Today,"DataIncludingGridded.Rdata",sep="_"))
write.csv(GridRainAllDataout,
          file="DataIncludingGridded.csv",
          row.names=F)




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
"flow_rain_maxT_weekly.csv: a dataframe with column headers Date, decade, station, Flow, Rain, maxT, stations are stacked",
"DataIncludingGridded.csv: a dataframe with column headers Date, decade, station, Flow, Rain, maxT, gridRain, stations are stacked"
)
writeLines(text,zz)
README_DataDescribe <- readLines(zz)
close(zz)

# write as an RData file
save("README_DataDescribe","flow_rain_maxT_weekly","CC","flow_zoo",
     "rain_zoo","maxT_zoo","GridRainAllDataout",file=paste(Today,"ClimCh_project_MD.Rdata",sep="_"))
# create also a zip file
zip(paste(Today,"ClimCh_project_MD.zip",sep="_"),
    c("README_DataDEscription.txt","flow_rain_maxT_weekly.csv",
      "20150829_CatchmentCharact.csv","DataIncludingGridded.csv"))