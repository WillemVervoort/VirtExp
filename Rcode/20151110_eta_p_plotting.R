# plotting eta_p values

# Now do some plotting
OutputChiewSH <- read.csv(paste(Today,"_SimHydHPC_ChiewAnalysis.csv",sep=""))
OutputChiewGR4J <- read.csv(paste(Today,"_GR4JHPC_ChiewAnalysis.csv",sep=""))
non_par_eta <- read.csv(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
                              Today,"_non_par_eta.csv",sep=""))
non_par_eta <- cbind(1:nrow(non_par_eta),non_par_eta)


OutputChiewSH$mod <- "SimHyd"
OutputChiewGR4J$mod <- "GR4J"



OutputChiew <- cbind(rbind(OutputChiewSH,OutputChiewGR4J),
                     rep(rep(non_par_eta$eta_p,each=10),2))
colnames(OutputChiew)[6] <- "np_eta_p"
# printing
p <- ggplot(OutputChiew, aes(x = station, y = eta_p)) +
  geom_boxplot(col="blue") +  facet_wrap(~ mod,ncol=1) +
  stat_summary(fun.y=mean, geom="point", shape=16, size=5,col="red",lwd=2)

p <- p +  geom_point(aes(x=station, y=np_eta_p),
           shape=16,size=5,colour="darkgreen") + facet_wrap(~ mod,ncol=1)
p <- p + ggtitle("Rainfall elasticity") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Station") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Rainfall Elasticity") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
  theme(legend.text = element_text( size = 12))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))

save(p,file=paste(Today,"_RainfallElasticityPlot.RData"))


# Normal quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_RainfallElasticityPlot.tif",sep=""),width=720,height=720)
print(p)
dev.off()



# publication quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_RainfallElasticityPlot.tif",sep=""),width=16*480,height=12*480,
     res=600, compression="lzw")
print(p)
dev.off()

OutputTrendsSH <- read.csv(paste(Today,"_SimHydHPC_trendsAnalysis.csv",sep=""))
OutputTrendsGR4J <- read.csv(paste(Today,"_GR4JHPC_trendsAnalysis.csv",sep=""))

OutputTrends <- rbind(OutputTrendsSH,OutputTrendsGR4J)
OutputTrends$mod <- c(rep("SimHyd",130),rep("GR4J",130))


p <- ggplot(OutputTrends, aes(x = station, y = Mod.r.sq)) +
  geom_boxplot(col="blue") +  facet_wrap(~ mod,ncol=1) +
  stat_summary(fun.y=mean, geom="point", shape=16, size=5,col="red",lwd=2)
p <- p + ggtitle("Model Calibration NSE") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Station") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Nash Sutcliffe Efficiency") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
  theme(legend.text = element_text( size = 12))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))

save(p,file=paste(Today,"_ModelCalStatsPlot.RData"))


# Normal quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_ModelCalStats.tif",sep=""),width=720,height=720)
print(p)
dev.off()



# publication quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_ModelCalStats.tif",sep=""),width=16*480,height=12*480,
     res=600, compression="lzw")
print(p)
dev.off()


OutputTrends$Tsig <- ifelse(OutputTrends$p_trend < 0.05,1,0)
OutputTrends$MKsig <- ifelse(OutputTrends$p_tau < 0.05,1,0)

# I need to make one df wich stacks MK and trend
names(OutputTrends)
One  <- with(OutputTrends,data.frame(station=station,trend=trend,p_trend=p_trend,mod))
One$m <- "Linear Trend"
Two  <- with(OutputTrends,data.frame(station=station,trend=tau,p_trend=p_tau,mod))
Two$m <- "Mann-Kendall"
colnames(Two) <- colnames(One)
plot.df <- rbind(One,Two)
plot.df$inter <- interaction(plot.df$station,plot.df$m)
plot.df$sig <- ifelse(plot.df$p_trend < 0.05,1,0)



# Now plot the actual trends
p <- ggplot(plot.df, aes(x = station, y = trend,col=sig)) +
  scale_colour_continuous(name="significance",low="white", high="black")  +
  geom_boxplot() +  facet_wrap(~ mod + m,ncol=1,scales="free")

p <- p + ggtitle("Residual trends: predicted - observed 1980 - 2010") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  xlab("Station") +
  theme(axis.title.x = element_text(face="bold",  size=16),
        axis.text.x  = element_text(size=12)) +
  ylab("Trend estimate or Mann Kendall tau") +
  theme(axis.title.y = element_text(face="bold",  size=16),
        axis.text.y  = element_text(size=12)) +
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(strip.text.x = element_text(size=16))

save(p,file=paste(Today,"_ModelResidualTrendPlot.RData"))


# Normal quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_ModelResidualTrendPlot.tif",sep=""),width=720,height=720)
print(p)
dev.off()



# publication quality
tiff(paste("C:/Users/rver4657/ownCloud/Virtual Experiments/",
           Today,"_ModelResidualTrendPlot.tif",sep=""),width=16*480,height=12*480,
     res=600, compression="lzw")
print(p)
dev.off()
