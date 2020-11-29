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
# GR4J stations
load(file="Data/GR4JHPCresiduals.Rdata")
resid_GR4J_st <- Residuals
# GR4J gridded
load(file="Data/GR4JGridHPCresiduals.Rdata")
resid_GR4J_grid <- Residuals

# SimHyd stations
load(file="Data/SimHydHPCresiduals.Rdata")
resid_SH_st <- Residuals

# SimHyd gridded
load(file="Data/SimHydGridHPCresiduals.Rdata")
resid_SH_grid <- Residuals


nc <- length(flow_stns) # number of cores
cl <- makeCluster( nc)
registerDoParallel(cl)

# ----------------------------------------------------------
# Residuals for model fits
# run in double parallel
i <- 2
data_in <- resid_GR4J_st[[i]]
# Store_resid = foreach(data_in = resid_GR4J_st, i = icount(),
#                 .packages = "HKprocess") %:%
# {
  Station_resid = foreach(j = icount(),
                                .packages = "HKprocess",.combine='rbind',
								.inorder = FALSE) %dopar%
  { 
  data_run <- data_in[,j]
	data_run[is.na(data_run)] <- 0
    run <- MannKendallLTP(as.numeric(data_run))
  }
#}
# save file
save(Station_resid,file=paste(flow_stns[i],"Store_GR4J_st.Rdata",sep="_"))


# Store_resid = foreach(i = 1:nc,
                      # .packages = "HKprocess") %:% {
data_in <- resid_GR4J_grid[[i]]

Station_resid = foreach(j = icount(),
                        .packages = "HKprocess",.combine='rbind',
                        .inorder = FALSE) %dopar%
                        { 
                          data_run <- data_in[,j]
                          data_run[is.na(data_run)] <- 0
                          run <- MannKendallLTP(as.numeric(data_run))
                        }
# }
# # save file
save(Station_resid,file=paste(flow_stns[i],"Store_GR4J_grid.Rdata",sep="_"))


# SimHyd
data_in <- resid_SH_st[[i]]

Station_resid = foreach(j = icount(),
                        .packages = "HKprocess",.combine='rbind',
                        .inorder = FALSE) %dopar%
                        { 
                          data_run <- data_in[,j]
                          data_run[is.na(data_run)] <- 0
                          run <- MannKendallLTP(as.numeric(data_run))
                        }
# # save file
save(Station_resid,file=paste(flow_stns[i],"Store_SH_st.Rdata",sep="_"))


data_in <- resid_SH_grid[[i]]

Station_resid = foreach(j = icount(),
                        .packages = "HKprocess",.combine='rbind',
                        .inorder = FALSE) %dopar%
                        { 
                          data_run <- data_in[,j]
                          data_run[is.na(data_run)] <- 0
                          run <- MannKendallLTP(as.numeric(data_run))
                        }
# save file
save(Station_resid,file=paste(flow_stns[i],"Store_SH_st.Rdata",sep="_"))
