#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################
# look at differences between gridded and station rainfall
# xy plot, qqplot, acf

setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")

require(zoo)
require(xts)
require(ggplot2)
Today <- format(Sys.Date(),"%Y%m%d")

load("ProjectData/20160804_ClimCh_project_MD.Rdata")
load("ProjectData/GriddedRainfallData.Rdata")
Gridrain_zoo <- output.z

# make a simple xyplot by catchment
xyp_rain <- ggplot(data=GridRainAllDataout,aes(x=Rain,y=gridRain,col=decade))
xyp_rain <- xyp_rain + geom_point() + facet_wrap(~Station)
tiff(paste("manuscriptdrafts/",
           Today,"_xyplotRain_gridRain.tif",sep=""),width=720,height=480)
print(xyp_rain)
dev.off()     

# make a qqplot
qqGridRain <- do.call(cbind,tapply(GridRainAllDataout$gridRain,
                               GridRainAllDataout$Station,
                               FUN=quantile,seq(0,1,length=1000)))
qqRain <- do.call(cbind,tapply(GridRainAllDataout$Rain,
                               GridRainAllDataout$Station,
                               FUN=quantile,seq(0,1,length=1000),na.rm=T))
qqGridRain_s <- melt(qqGridRain,measure.vars=1:13)
qqRain_s <- melt(qqRain,measure.vars=1:13)
Rainqq <- data.frame(Station=qqRain_s$X2,qqRain=qqRain_s$value,
                     qqGridRain=qqGridRain_s$value)
xyp_rainqq <- ggplot(data=Rainqq,aes(x=qqRain,y=qqGridRain))
#xyp_rainqq <- xyp_rainqq 
xyp_rainqq <- xyp_rainqq + geom_point() + facet_wrap(~Station)
xyp_rainqq <- xyp_rainqq + geom_abline(intercept = 0, slope = 1,
                            col="darkred",linetype="dashed",size=1)

tiff(paste("manuscript drafts/",
           Today,"_qqplotRain_gridRain.tif",sep=""),width=720,height=480)
print(xyp_rainqq)
dev.off()     

# Now show acfs
# Gridded rainfall
gridacf <- tapply(GridRainAllDataout$gridRain,
                             GridRainAllDataout$Station,
                             acf, plot = FALSE)
gridacfdf <- do.call(rbind,lapply(gridacf,function(x) with(x,data.frame(lag, acf))))
gridacfdf$Station <- rep(unique(GridRainAllDataout$Station),each=34)


acfGrid <- ggplot(data = gridacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  facet_wrap(~Station)
tiff(paste("manuscript drafts/",
           Today,"_acfplot_gridRain.tif",sep=""),width=720,height=480)
acfGrid
dev.off()
# Now normal rainfall
rainacf <- tapply(GridRainAllDataout$Rain,
                  GridRainAllDataout$Station,
                  acf, plot = FALSE,na.action=na.pass)
rainacfdf <- do.call(rbind,lapply(rainacf,function(x) with(x,data.frame(lag, acf))))
rainacfdf$Station <- rep(unique(GridRainAllDataout$Station),each=34)


acfRain <- ggplot(data = rainacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  facet_wrap(~Station)
tiff(paste("manuscript drafts/",
           Today,"_acfplot_Rain.tif",sep=""),width=720,height=480)
acfRain
dev.off()
# no difference in acf
