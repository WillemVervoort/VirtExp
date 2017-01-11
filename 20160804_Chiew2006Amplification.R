#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# Amplification a la Francis Chiew.
# Following analysis in Chiew (2006)

##################
##  ~ Set up ~  ##
##################
# SET WORKING DIRECTORY # #####
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/Manuscript drafts")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(ggplot2)
require(lattice)
require(mgcv)
require(hydromad)
#####
# STATIONS # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", 
                        "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502",
                        "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", 
                        "YARR"="614044", "DOMB"="607155")

# read in the data
load("../ProjectData/20160804_ClimCh_project_MD.Rdata")


# Use Chiew 2006 and non parametric

# Aggregate flow and rain to annual data
annualF <- aggregate(flow_zoo,format(time(flow_zoo),"%Y"),sum,na.rm=T)
mean_annualF <- apply(annualF,2,mean,na.rm=T)

annualR <- aggregate(rain_zoo,format(time(rain_zoo),"%Y"),sum,na.rm=T)
mean_annualR <- apply(annualR,2,mean,na.rm=T)

# Check cumulative running anomalies
mean_dailyR <- apply(rain_zoo,2,mean,na.rm=T)
store <- list()


# cumulative sums
for (i in 1:ncol(rain_zoo)) {
   x <- rain_zoo[,i]
   miss <- is.na(rain_zoo[,i])
   #x[miss] <- 0
  cs <- cumsum(ifelse(is.na(rain_zoo[,i]-mean_dailyR[i])==T,
                      0, x - mean_dailyR[i]))
  cs[miss] <- NA
  store[[i]] <- data.frame(Date=time(rain_zoo),data=as.numeric(cs),
                           station=rep(names(flow_stns)[i],length(cs)))
#   if(i==1) {
#     plot(cs,ylim=c(-16000,2000))  
#   } else {
#     lines(cs,lty=i,col=i)
#   }
  rain_zoo[miss,i] <- NA
}
# lgd.txt <- colnames(rain_zoo)
# legend("bottomleft",lgd.txt,col=1:ncol(rain_zoo),lty=1:ncol(rain_zoo))
plot.df <- data.frame(do.call(rbind,store))
ggplot(plot.df,aes(x=Date,y=data)) + geom_line() + facet_wrap(~station)
# No clear indication of a trend


annualT <- aggregate(maxT_zoo,format(time(rain_zoo),"%Y"),mean,na.rm=T)
mean_annualT <- apply(annualT,2,mean,na.rm=T)


# Non parametric
eta_p <- function(Q,P,meanQ,meanP) {median((Q-meanQ)/(P-meanP)*meanP/meanQ)}

out <- list()
for (i in 1:ncol(annualF)) {
  out[[i]] <- eta_p(annualF[,i], annualR[,i],mean_annualF[i],mean_annualR[i])
}
non_par_eta <- data.frame(stn = colnames(flow_stns),eta_p=do.call(c,out))

# now calculate annual slopes to check
# First rearrange the annual data to a dataframe for GAM
flow_an_stack <- stack(as.data.frame(annualF))
rain_an_stack <- stack(as.data.frame(annualR))
maxT_an_stack <- stack(as.data.frame(annualT))

gam_an_df <- data.frame(stn =substr(flow_an_stack$ind,1,4),flow=flow_an_stack$values,
                        rain=rain_an_stack$values,maxT=maxT_an_stack$values,
                        trend = rep(1:nrow(annualF),ncol(annualF))) 

# xyplot(flow~rain|stn,data=gam_an_df,
#        panel=function(x, y){
#          panel.xyplot(x, y, pch=16)
#          panel.lines(c(0,4000), c(0,4000), lty=2,col="black")
# }, as.table=T)

p <- ggplot(gam_an_df, aes(x = rain, y = flow)) +
  geom_point(size=4,col="black") + 
  geom_abline(intercept=0, slope=1,col="grey50",lty=2,lwd=2) +
  facet_wrap(~ stn,ncol=5) +
  guides(col = guide_legend(nrow = 3))
p <- p + ggtitle("Rainfall - Runoff") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Rainfall") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Runoff") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
  theme(legend.text = element_text( size = 12))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))

save(p,file=paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",Today,"_RainfallRunoffPlot.RData",sep=""))
load("C:/Users/rver4657/ownCloud/Virtual Experiments/20160218_RainfallRunoffPlot.Rdata")


# Normal quality
# tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
#            Today,"_RainfallRunoffPlot.tif",sep=""),width=720,height=720)
# print(p)
# dev.off()



# publication quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_RainfallRunoffPlot.tif",sep=""),width=16*480,height=12*480,
     res=600, compression="lzw")
print(p)
dev.off()
# Save the data
write.csv(gam_an_df,
      file=paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
                Today,"_RainfallRunoff.csv",sep=""),row.names=F)
write.csv(non_par_eta,
          file=paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
                     Today,"_non_par_eta.csv",sep=""),row.names=F)

