#################################
### CLIMATE CHANGE STREAM FLOW ##
###         ~ Week 3 ~         ##
#################################

# From 20150830 flow data converted from ML/day to mm
# This version uses gridded rainfall rather than station data
# reanalyses GAM(flow ~ P + trend) to match Chiew et al.
# and GLS(flow ~ trend) and GLS(Rain ~ trend)
# the idea here is that the linear trend in the streamflow would show the amplification

# IMPORT FLOW DATA INTO R # #####
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")

require(zoo)
require(xts)
Today <- format(Sys.Date(),"%Y%m%d")

load("ProjectData/20160804_DataIncludingGridded.Rdata")


# Flow stations
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", 
                        "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", 
                        "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497",
                        "MURR"="405205", "SOUT"="225020A", "YARR"="614044", 
                        "DOMB"="607155")

#####

## GAMM analysis
library(mgcv)
library(ggplot2)
# we cannot use gamm4 as this will not allow correlation structures and no te()


# 1. GAMM with rain & s(rain,MaxT)
# **************************
# ANALYSE resiudals using MK
# **************************

#with corr
GridRainAllData.na_removed <- na.omit(GridRainAllDataout)
for (i in seq_along(flow_stns)) {
    gamm.data <- subset(GridRainAllDataout, Station==colnames(flow_stns[i]))
#  gamm.data$lagRain <- lag(gamm.data$Rain)
#  gamm.data <- na.omit(gamm.data)
#  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gamm(log(Flow+1)~s(gridRain) + s(gridRain,MaxT), 
        corr = corCAR1(), data=gamm.data, control=list(niterEM=5)))
  print(i)
}

gamm.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list[[i]] <- get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))
}
save(gamm.list,file=paste(Today,"gamm.grid_noLag.Rdata",sep="_"))

load("20160812_gamm.grid_noLag.Rdata")

tiff(paste("manuscript drafts/",Today,"gammgrid_residuals_noLag.tif"),
     width=480,height=720)
par(mfrow=c(5,3))
for (i in seq_along(flow_stns)) {
  plot(residuals(gamm.list[[i]]$lme,
                 type="normalized"), main=colnames(flow_stns[i]),
       ylab="normalised residuals")
  n <- length(residuals(gamm.list[[i]]$lme,type="normalized"))
  abline(lsfit(1:n, residuals(gamm.list[[i]]$lme,type="normalized")), col="red")
}
dev.off()
# look at results
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  print(summary(gamm.list[[i]]$gam)$s.table[,4])
  print(summary(gamm.list[[i]]$gam)$r.sq)
}
# look at results
Store <- data.frame(p_smooth_rain=numeric(length(flow_stns)),
                    p_smooth_int=numeric(length(flow_stns)),
                    r_sq = numeric(length(flow_stns)))
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  Store[i,1:2] <- summary(gamm.list.t[[i]]$gam)$s.table[,4]
  Store[i,3] <- summary(gamm.list.t[[i]]$gam)$r.sq
}

Store$stn <- colnames(flow_stns)



# do mann kendall on the residuals
save(hp, file=paste(Today,"_FigGridResidGAM.Rdata",sep=""))
load("20160806_FigGridResidGAM.Rdata")
require(Kendall)
require(ggplot2)
resid.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  resid.list[[i]] <- zoo(residuals(gamm.list[[i]]$lme,
                 type="normalized"),
                 order.by=as.Date(na.omit(subset(GridRainAllDataout, 
                                                 Station==colnames(flow_stns[i])))$Date))
}
resid.df <- do.call(merge.zoo,resid.list)
names(resid.df) <- colnames(flow_stns)
# Bootstrap
set.seed(10)
# now run a loop over the number of years (create 41 different sets)
# do Mann Kendall test on each resonstituted series
# ---------------------------
#  -------------------------
resid.temp <- as.data.frame(resid.df)
resid.temp$years <- format(time(resid.df),"%Y")
split.resid <- split(resid.temp[,1:13],resid.temp$years)

MK.list <- list()

for (i in 1:500) {
  #i <- 1
  # reorganise the list elements
  series <- sample(1:nyears(resid.df),nyears(resid.df))
  for (j in 1:length(series)) {
    #j <- 1
    if (j==1) { 
      new.df <- as.data.frame(split.resid[[series[j]]])
    } else {
      new.df <- rbind(new.df,as.data.frame(split.resid[[series[j]]]))
    }
    #new.list[[j]] <- split.flow[[series[j]]]
  }
  # rbind to dataframe
  #if(i == 1) plot(new.df[,1], type="l") else lines(new.df[,1],col=i)
  # run mann kendall on the columns and store the results
  mk.r <- apply(new.df,2,MannKendall)
  
  MK.list[[i]] <- do.call(cbind,mk.r)  
}

MK.df <- do.call(rbind,MK.list)
#hist(as.numeric(MK.df[row.names(MK.df)=="tau","COEN"]))

pvalues <- subset(MK.df, rownames(MK.df)=="sl")
tau <- subset(MK.df, rownames(MK.df)=="tau")

sig.set <- list()

for (i in 1:ncol(pvalues)) {
  #i <- 1
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
                    tau=as.numeric(tau[,i]),catch=rep(colnames(MK.df)[i],nrow(tau)))
  sig.set[[i]] <- set[set$pvalue < 0.5,]
}

sig.set.a <- do.call(rbind,sig.set)
sig.set.a$type <- rep("bootstrap",nrow(sig.set.a))


MK.resid <- do.call(rbind,lapply(resid.list,MannKendall))


real.df <- data.frame(pvalue = as.numeric(MK.resid[,2]), 
                      tau = as.numeric(MK.resid[,1]),
                      catch=colnames(flow_stns),
                      type=rep("real",nrow(MK.resid)))
# A histogram of taus

hp <- ggplot(sig.set.a, aes(x=tau)) + geom_histogram(binwidth=0.03,colour="white")
#print(hp)
# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp <- hp + facet_wrap(~ catch,ncol=5)
# add a red point for the real slope from the data
p_value <- ifelse(real.df$pvalue<0.05,"< 0.05",">= 0.05")
hp <- hp + geom_point(data=real.df,aes(x=tau, y=0,colour=p_value),
                shape=16,size=5) + 
  facet_wrap(~ catch,ncol=5)+ ggtitle("Residuals Streamflow after GAM") #+
hp <- hp + scale_colour_grey(start = 0, end = 0.6)

# # production quality
# tiff(paste("manuscript drafts/",Today,"_FigGridResidGAM.tif",sep=""),res=600,compression="lzw",
#      width=10*480,height=10*480)
# quicktiff
tiff(paste("manuscript drafts/",Today,"_DraftFigGridResidGAM.tif",sep=""),
     width=960,height=960)
print(hp)
dev.off()
# -------- end no 1. -----------------


# 2. GAMM with both and trend
#*******************************************
# INSERT TREND IN MODEL
# analyse trend
#*******************************************

#with corr and trend
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(GridRainAllDataout, Station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  #  gamm.data$lagRain <- lag(gamm.data$Rain)
  #  gamm.data <- na.omit(gamm.data)
  #  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gamm(log(Flow+1)~ trend + s(gridRain) + s(gridRain,MaxT), 
              corr = corCAR1(), data=gamm.data, control=list(niterEM=0)))
  print(i)
}
# Put the gamm results in a list and store
gamm.list.t <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list.t[[i]] <- get(paste("flow_weekly_gam.temp.", 
                                colnames(flow_stns[i]),sep=""))
}
save(gamm.list.t,file=paste(Today,"gamm.grid_noLagTrend.Rdata",sep="_"))


# read in the GAMM results to look at rsq adj
load("20160806_gamm.grid_noLagTrend.Rdata")

# # look at results
# for (i in seq_along(flow_stns)) {
#   print(colnames(flow_stns[i]))
#   print(summary(gamm.list.t[[i]]$gam)$p.table[,c(1,4)])
#   print(summary(gamm.list.t[[i]]$gam)$s.table[,4])
#   print(summary(gamm.list.t[[i]]$gam)$r.sq)
# }

# look at results
Store <- data.frame(trend=numeric(length(flow_stns)),
                     p_trend=numeric(length(flow_stns)),
                     p_smooth_rain=numeric(length(flow_stns)),
                    p_smooth_int=numeric(length(flow_stns)),
                    r_sq = numeric(length(flow_stns)))
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  Store[i,1:2] <- summary(gamm.list.t[[i]]$gam)$p.table[2,c(1,4)]
  Store[i,3:4] <- summary(gamm.list.t[[i]]$gam)$s.table[,4]
  Store[i,5] <- summary(gamm.list.t[[i]]$gam)$r.sq
}

Store$stn <- colnames(flow_stns)


# create data frames for the replotting of the interaction plots
plot.l2 <- list()
# extract the plot data
for (i in seq_along(flow_stns)) {
  test <- plot.gam(gamm.list.t[[i]]$gam, pages =1, select=2)
  plot.l2[[i]] <- data.frame(fit=test[[2]]$fit, x=rep(test[[2]]$x,40),
                        y = sort(rep(test[[2]]$y,40)), 
                        station = names(flow_stns)[i])
}  
plot.df2 <- do.call(rbind,plot.l2)
require(lattice)

# Plotting
# Make a nice levelplot for the interactions
# Publication quality
# tiff(paste("manuscript drafts/",Today,"_TrendsMaxTRainGammGrid.tif",sep=""),res=600,compression="lzw",
#      width=10*480,height=10*480)

# draft quality
tiff(paste("manuscript drafts/",Today,"_TrendsMaxTRainGammGrid.tif",sep=""),
     width=2*480,height=2*480)

trellis.par.set("regions", list(col=heat.colors(100)))
trellis.par.set("background", list(col="white"))
levelplot(fit~x+y|station, data=plot.df2, scales="free",
          xlab=list("Weekly rainfall (mm)",font=2,cex=1.2),
          ylab=list("Weekly maximum T (C)",font=2,cex=1.2),
          contour=T)
dev.off()


# USe ggplot to plot the smooths
# This is some code I found somewhere, but had to tweak
# create data frames for the replotting of the interaction plots
plot.l1 <- list()
points.l1 <- list()
# extract the plot data
for (i in seq_along(flow_stns)) {
  png(paste0(tempdir(),"/temporary_R_file.png"))
    plot.temp <- plot.gam(gamm.list.t[[i]]$gam, residuals=T,pages =1, select=1)
  dev.off()
    
  plot.l1[[i]] <- data.frame(x=plot.temp[[1]]$x, smooth=plot.temp[[1]]$fit, 
                             se=plot.temp[[1]]$se, 
                             station = names(flow_stns)[i])
  points.l1[[i]] <- data.frame(data.x=plot.temp[[1]]$raw, data.y=plot.temp[[1]]$p.resid, 
          station = names(flow_stns)[i])

}  
plot.df1 <- do.call(rbind,plot.l1)
points.df1 <- do.call(rbind,points.l1)

# make a fancy plot of the smooths

p <-    ggplot(plot.df1, aes(x, smooth)) + geom_line(col="black", size=1) +
    geom_point(data = points.df1, aes(data.x, data.y), col="gray50", size=1, alpha=I(0.2)) +    
    geom_line(data=plot.df1,aes(y=smooth-se), linetype="dashed", col="red", size=1) + 
    geom_line(data=plot.df1,aes(y=smooth+se), linetype="dashed", col="red", size=1) + 
      facet_wrap(~station,scales="free") + theme_bw() + xlab("Rainfall (mm)") +
      ylab("Response relative to mean") +
      theme(axis.title.x = element_text(face="bold",  size=12), 
            axis.title.y = element_text(face="bold",  size=12))

save(p,file=paste(Today,"_TrendsRainGamm.Rdata",sep=""))

# publication quality
#tiff(paste("manuscript drafts/",Today,"_TrendsRainGamm.tif",sep=""),res=600,compression="lzw",
#     width=10*480,height=10*480)

# draft quality
tiff(paste("manuscript drafts/",Today,"_TrendsRainGamm.tif",sep=""),
     width=2*480,height=2*480)
print(p)

dev.off()
# ----------- end no 2. ----------------

      

# 3. new at 3/9/2015
# ******************************
# GAMM with rainfall and trend
# *****************************
#with corr
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(GridRainAllDataout, Station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gamm(log(Flow+1)~s(gridRain) + trend, 
              corr = corCAR1(), data=gamm.data, control=list(niterEM=10)))
  print(i)
}

gamm_RainOnly.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm_RainOnly.list[[i]] <- get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))
}
save(gamm_RainOnly.list,file=paste(Today,"gamm.grid_RainOnly.Rdata",sep="_"))

load("20160812_gamm.grid_RainOnly.Rdata")

tiff(paste("manuscript drafts/",Today,"gamm_residuals__RainOnly.tif"),
     width=480,height=720)
par(mfrow=c(5,3))
for (i in seq_along(flow_stns)) {
    res <- residuals(gamm_RainOnly.list[[i]]$lme,
                     type="normalized")  
    plot(res, main=colnames(flow_stns[i]),
         ylab="normalised residuals")
    n <- length(res)
    abline(lsfit(1:n, res), col="red")
}
dev.off()
# look at results
Store1 <- data.frame(trend=numeric(length(flow_stns)),
                     p_trend=numeric(length(flow_stns)),
                     p_smooth=numeric(length(flow_stns)),
                     r_sq = numeric(length(flow_stns)))
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  Store1[i,1:2] <- summary(gamm_RainOnly.list[[i]]$gam)$p.table[2,c(1,4)]
  Store1[i,3] <- summary(gamm_RainOnly.list[[i]]$gam)$s.table[,4]
  Store1[i,4] <- summary(gamm_RainOnly.list[[i]]$gam)$r.sq
}

Store1$stn <- colnames(flow_stns)


# Save
save(Store1,file=paste(Today,"Store1_GridRain&TrendOnlyAnalysis.RData",sep="_"))

load("20160806_Store1_GridRain&TrendOnlyAnalysis.RData")
Store1

# ----------- end no 3. ----------------

# 4. Look at the trend in flow
# Only flow and trend
# new at 5/9/2015
# ******************************
# GAMM with just trend
# *****************************
# Use gls, to just look at the flow trend
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(GridRainAllDataout, Station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  assign(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gls(log(Flow +1)~trend, correlation= corCAR1(),
       data=gamm.data))
print(i)
}

#save(gamm.data,file="testdata.Rdata")


gamm_TrendOnly.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm_TrendOnly.list[[i]] <- get(paste("flow_weekly_gam.temp.", colnames(flow_stns[i]), sep=""))
}
save(gamm_TrendOnly.list,file=paste(Today,"gamm.grid_TrendOnly.Rdata",sep="_"))

load("20160806_gamm.grid_TrendOnly.Rdata")

tiff(paste("manuscript drafts/",Today,"gam_residuals__TrendOnly.tif"),
     width=480,height=720)
par(mfrow=c(5,3))
for (i in seq_along(flow_stns)) {
  res <- residuals(gamm_TrendOnly.list[[i]])  
  plot(res, main=colnames(flow_stns[i]),
       ylab="normalised residuals")
  n <- length(res)
  abline(lsfit(1:n, res), col="red")
}
dev.off()
# look at results
Store2 <- data.frame(trend=numeric(length(flow_stns)),
                     p_trend=numeric(length(flow_stns)),
#                     p_smooth=numeric(length(flow_stns)),
                     AIC = numeric(length(flow_stns)))
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  Store2[i,1:2] <- summary(gamm_TrendOnly.list[[i]])$tTable[2,c(1,4)]
#  Store1[i,3] <- summary(gamm_RainOnly.list[[i]]$gam)$s.table[,4]
  Store2[i,3] <- summary(gamm_TrendOnly.list[[i]])$AIC
}

Store2$stn <- colnames(flow_stns)
save(Store2,file=paste(Today,"Store2_gridTrendOnlyAnalysis.RData",sep="_"))

load("20160806_Store2_gridTrendOnlyAnalysis.RData")
# ----------- end 4. ------------------

# 5. Look at the trend in rain
# Only rain and trend
# new at 5/9/2015
# ******************************
# GAMM with just trend
# *****************************
# Use gls, to just look at the flow trend
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(GridRainAllDataout, Station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  assign(paste("Rain_weekly_gls.temp.", colnames(flow_stns[i]), sep=""), 
         gls(log(gridRain +1)~trend, correlation= corCAR1(),
             data=na.omit(gamm.data)))
  print(i)
}

#save(gamm.data,file="testdata.Rdata")


gamm_RainTrendOnly.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm_RainTrendOnly.list[[i]] <- get(paste("Rain_weekly_gls.temp.", colnames(flow_stns[i]), sep=""))
}
save(gamm_RainTrendOnly.list,file=paste(Today,"gamm.grid_RainTrendOnly.Rdata",sep="_"))

load("20160806_gamm.grid_RainTrendOnly.Rdata")

# look at results
Store3 <- data.frame(trend=numeric(length(flow_stns)),
                     p_trend=numeric(length(flow_stns)),
                     #                     p_smooth=numeric(length(flow_stns)),
                     AIC = numeric(length(flow_stns)))
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  Store3[i,1:2] <- summary(gamm_RainTrendOnly.list[[i]])$tTable[2,c(1,4)]
  #  Store1[i,3] <- summary(gamm_RainOnly.list[[i]]$gam)$s.table[,4]
  Store3[i,3] <- summary(gamm_RainTrendOnly.list[[i]])$AIC
}

Store3$stn <- colnames(flow_stns)
save(Store3,file=paste(Today,"Store3_gridRainTrendOnly.RData",sep="_"))

load("20160806_Store3_gridRainTrendOnly.RData")

# ------end 5. -------------------