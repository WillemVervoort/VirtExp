##---------------------------
# sub script to run Mankendall under LTP assumptions
# each weekly series takes 18 hours on an single core i7
# Running parallel on HPC
# Virtual experiments project/trend analysis in catchments
# Willem Vervoort, Michaela Dolk, Floris van Ogtrop
# 2018

setwd("/project/RDS-FSC-CCH-RW/MDProjectdata")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
library(doParallel)
require(HKprocess)
require(xts)
require(zoo)
#####

#####
# STATIONS # #####
flow_stns <- c("COTT", "RUTH", "CORA", "ELIZ", "COCH", "COEN", "SCOT", "HELL", "NIVE", "MURR", "SOUT", "YARR", "DOMB")

# read in the data
load("Data/ClimCh_project_MD.Rdata")
load("Data/DailyDataIncludingGridded.Rdata")
GridRain <- GridRainAllDataout
#rm(flow_rain_maxT_weekly)
#rm(CC)

nc <- length(flow_stns) # number of cores
registerDoParallel(cores=nc) 

# first run on raw data (without deseasonalising)

# ----------------------------------------------------------
# Step 1: Summarise to weekly
# flow (sum flow)
flow_xts <- xts(flow_zoo,
                order.by=time(flow_zoo),
                frequency=1)

# weekly data summarising (destroys xts object)
flow_weekly <- apply(flow_xts,2,
                     function(x) apply.weekly(x,
                                              sum,na.rm=T))
# define weekly dates
Dates <- time(apply.weekly(flow_xts[,1],sum))
# restore the xts object
flow_weekly_xts <- as.xts(flow_weekly,
                          order.by=Dates)


 # rainfall (sum rainfall)
 rainfall_xts <- xts(rain_zoo,
                     order.by=time(rain_zoo),
                     frequency=1)
 # weekly data summarising (destroys xts object)
 rainfall_weekly <- apply(rainfall_xts,2,
                          function(x) apply.weekly(x,
                                                   sum,na.rm=T))
 # define weekly dates
 Dates <- time(apply.weekly(rainfall_xts[,1],sum))
 # restore the xts object
 rainfall_weekly_xts <- as.xts(rainfall_weekly,
                               order.by=Dates)
 
 # gridded rainfall
 gridrainfall_xts <- xts(gridRain_zoo, 
                     order.by=time(gridRain_zoo), 
                     frequency=1)
 # weekly data summarising (destroys xts object)
 gridrainfall_weekly <- apply(gridrainfall_xts,2,
                          function(x) apply.weekly(x,
                                                   sum,na.rm=T))
 # define weekly dates
 Dates <- time(apply.weekly(gridrainfall_xts[,1],sum))
 # restore the xts object
 gridrainfall_weekly_xts <- as.xts(gridrainfall_weekly,
                               order.by=Dates)
 

# rerun MaxT
# maxT
maxT_xts <- xts(maxT_zoo, 
                order.by=time(maxT_zoo), 
                frequency=1)
# weekly data summarising (destroys xts object)
maxT_weekly <- apply(maxT_xts,2,
                     function(x) apply.weekly(x,
                                              mean,na.rm=T))
# define weekly dates
Dates <- time(apply.weekly(maxT_xts[,1],sum))
# restore the xts object
maxT_weekly_xts <- as.xts(maxT_weekly,
                          order.by=Dates)
# ---------------------------------------------------------

# flow
# run in parallel
Store = foreach(j = 1:nc,
                .packages="HKprocess") %dopar%
{
  run <- MannKendallLTP(flow_weekly[,j])
}
# save file
save(Store,file="Store_flow.Rdata")


# rainfall
# run in parallel
Store_rain = foreach(j = 1:nc,
                     .packages="HKprocess") %dopar%
{
  run <- MannKendallLTP(rainfall_weekly[,j])
}
save(Store_rain,file="Store_rain.Rdata")

# gridded rainfall
# run in parallel
Store_gridRain = foreach(j = 1:nc,
                         .packages="HKprocess") %dopar%
{
  run <- MannKendallLTP(gridrainfall_weekly[,j])
}

save(Store_gridRain,file="Store_gridrain.Rdata")

# maxT
# run in parallel
Store_maxT = foreach(j = 1:nc,
                     .packages="HKprocess") %dopar%
{
  run <- MannKendallLTP(na.omit(maxT_weekly[,j]))
}

save(Store_maxT,file="Store_maxT.Rdata")
