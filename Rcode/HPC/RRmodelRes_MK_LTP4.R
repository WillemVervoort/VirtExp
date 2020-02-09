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
#####

#####
# STATIONS # #####
flow_stns <- c("COTT", "RUTH", "CORA", "ELIZ", "COCH", "COEN", "SCOT", "HELL", "NIVE", "MURR", "SOUT", "YARR", "DOMB")

# read in the data
# GR4J stations
#load(file="Data/GR4JHPCresiduals.Rdata")
#resid_GR4J_st <- Residuals
# GR4J gridded
#load(file="Data/GR4JGridHPCresiduals.Rdata")
#resid_GR4J_grid <- Residuals

# # SimHyd stations
# load(file="Data/SimHydHPCresiduals.Rdata")
# resid_SH_st <- Residuals
# 
# # SimHyd gridded
 load(file="Data/SimHydGridHPCresiduals.Rdata")
 resid_SH_grid <- Residuals

MK_analysis <- function(data_run) {
  data_run <- na.trim(data_run, sides="both")
  # SUMMARISE TO WEEKLY
  resid_weekly <- apply.weekly(data_run,sum,na.rm=T)
  # run MannKendallLTP
  run <- MannKendallLTP(as.numeric(resid_weekly))
  return(run)
}


nc <- 20 # number of cores
cl <- makeCluster(nc)
registerDoParallel(cl)

# ----------------------------------------------------------
# Residuals for model fits
# would like to run in double parallel
# combine all the stations together (130 columns)
#DataAll <- do.call(cbind,resid_GR4J_st)
#DataAll <- do.call(cbind,resid_GR4J_grid)
#DataAll <- do.call(cbind,resid_SH_st)
DataAll <- do.call(cbind,resid_SH_grid)

# select 2 stations at a time
data_in <- DataAll[,81:100]
# Store_resid = foreach(data_in = resid_GR4J_st, i = icount(),
#                 .packages = "HKprocess") %:%
# {
# Run 10 realisations
  Station_resid = foreach(j = 1:nc,
                                .packages = c("HKprocess","xts"),.combine='rbind',
								.inorder = TRUE) %dopar%
  { 
    require(HKprocess)
    require(xts)
    data_run <- xts(data_in[,j])
    MK_analysis(data_run)
  
  }
#}
stopCluster(cl)  
  
# save file
#save(Station_resid,file=paste(Sys.Date(),"Store_GR4J_st2.Rdata",sep="_"))
#save(Station_resid,file=paste(Sys.Date(),"Store_GR4J_gr5.Rdata",sep="_"))
#save(Station_resid,file=paste(Today,"Store_SH_st5.Rdata",sep="_"))
save(Station_resid,file=paste(Today,"Store_SH_gr5.Rdata",sep="_"))


