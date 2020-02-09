# analyses Hamed MK results, weekly

setwd("E:/cloudstor/Virtual Experiments/ProjectData/HPCresults")

require(HKprocess)
require(tidyverse)

# read in the results from the MannKendallLTP analysis
# start with flow
load("Store_flow.Rdata")
MK_standard <- lapply(Store, "[[","Mann_Kendall")
Hsignif <- lapply(Store, "[[","Significance_of_H")
MK_LTP <- lapply(Store, "[[","Mann_Kendall_LTP")

flow_MKLTP <- cbind(do.call(rbind, MK_standard),do.call(rbind, Hsignif),
                    do.call(rbind, MK_LTP))
colnames(flow_MKLTP)[c(6,8,10)] <- paste(c("MK","Hest","MKLTP"), 
                                         "2_sided_pvalue",sep="_")
flow_MKLTP

# rainfall
load("Store_rain.Rdata")
MK_standard <- lapply(Store_rain, "[[","Mann_Kendall")
Hsignif <- lapply(Store_rain, "[[","Significance_of_H")
MK_LTP <- lapply(Store_rain, "[[","Mann_Kendall_LTP")

rain_MKLTP <- cbind(do.call(rbind, MK_standard),do.call(rbind, Hsignif),
                    do.call(rbind, MK_LTP))
colnames(rain_MKLTP)[c(6,8,10)] <- paste(c("MK","Hest","MKLTP"), 
                                         "2_sided_pvalue",sep="_")
rain_MKLTP

# gridded rainfall
load("Store_gridrain.Rdata")
MK_standard <- lapply(Store_gridRain, "[[","Mann_Kendall")
Hsignif <- lapply(Store_gridRain, "[[","Significance_of_H")
MK_LTP <- lapply(Store_gridRain, "[[","Mann_Kendall_LTP")

gridrain_MKLTP <- cbind(do.call(rbind, MK_standard),do.call(rbind, Hsignif),
                    do.call(rbind, MK_LTP))
colnames(gridrain_MKLTP)[c(6,8,10)] <- paste(c("MK","Hest","MKLTP"), 
                                         "2_sided_pvalue",sep="_")
gridrain_MKLTP

# maxT
load("Store_maxT.Rdata")
MK_standard <- lapply(Store_maxT, "[[","Mann_Kendall")
Hsignif <- lapply(Store_maxT, "[[","Significance_of_H")
MK_LTP <- lapply(Store_maxT, "[[","Mann_Kendall_LTP")

maxT_MKLTP <- cbind(do.call(rbind, MK_standard),do.call(rbind, Hsignif),
                    do.call(rbind, MK_LTP))
colnames(maxT_MKLTP)[c(6,8,10)] <- paste(c("MK","Hest","MKLTP"), 
                                         "2_sided_pvalue",sep="_")
maxT_MKLTP
