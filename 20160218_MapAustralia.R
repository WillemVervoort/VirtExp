
#
setwd("C:\\Users\\rver4657\\ownCloud\\Virtual Experiments")

Today <- format(Sys.Date(),"%Y%m%d")
library(oz)
# MAP STATIONS # #####
Stations <- read.csv("For Willem_05_06_2015/Streamflow_data/Flow_station_locations.csv")
# production quality
tiff(paste(Today,"_Figure1_MapOZMDPaper.tif",sep=""),res=600,compression="lzw",
     width=10*480,height=10*480)
# quicktiff
# tiff(paste(Today,"_DraftMapOZ.tif",sep=""),
#      width=960,height=960)
oz(col="gray",lwd=2)
points(Stations$Longitude, Stations$Latitude, pch = 20, col="black", cex=1.2)
text(x=Stations$Longitude,y=Stations$Latitude,Stations$Station, cex=0.8, pos=c(2,4,4,4,4,4,4,2,4,2,4,2,2), offset=0.2)
dev.off()
#####
