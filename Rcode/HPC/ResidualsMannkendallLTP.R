##---------------------------
# sub script to run Mankendall under LTP assumptions
# each weekly series takes 18 hours on an single core i7
# Running parallel on HPC
# Virtual experiments project/trend analysis in catchments
# Willem Vervoort, Michaela Dolk, Floris van Ogtrop
# 2018

setwd("/project/RDS-FSC-CCH-RW/MDProjectdata")
#setwd("E:/Cloudstor/Virtual Experiments/VirtExp")
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
load("Data/GAMMmodel5Resid.rdata")
resid_st_df <- resid_df
load("Data/GAMMmodel5Resid_grid.rdata")
resid_grid_df <- resid_df

nc <- length(flow_stns) # number of cores
registerDoParallel(cores=nc) 

# first run on raw data (without deseasonalising)

# ----------------------------------------------------------
# This assumes that the residuals for FLOW == 0 are also 0
# Model5 station rain
# run in parallel
Store_resid = foreach(j = 1:nc,
.packages = "HKprocess") %dopar%
{
data_in <- resid_st_df[,j]
data_in[is.na(data_in)] <- 0
run <- MannKendallLTP(as.numeric(data_in))
}
# save file
save(Store_resid,file="Store_Res_Mod5.Rdata")


# # gridded rainfall
# # run in parallel
# Store_resid_grid = foreach(j = 1:nc,
#                      .packages = "HKprocess") %dopar%
# {
#   data_in <- resid_grid_df[,j]
#   data_in[is.na(data_in)] <- 0
#   run <- MannKendallLTP(as.numeric(data_in))
# }
# save(Store_resid_grid,file="Store_Res_Mod5_grid.Rdata")
# 
