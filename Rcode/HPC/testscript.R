setwd("/project/RDS-FSC-CCH-RW/MDProjectdata")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(hydromad)
library(doParallel)

data <- read.csv("Data/testdata.csv")

registerDoParallel(10)

test <- foreach(i = 1:10, .combine=rbind) %dopar% 
  {
  # read in a bit of the data
  subdata <- data[((i-1)*10 + 1):(i*10),]
  write.csv(subdata,paste("Data/subdata_",i,".csv",sep=""))
  }
