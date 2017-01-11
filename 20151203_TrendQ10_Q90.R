#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################

# Analyse changes in the flow variability and the FDC
# Q10, Q90 and CV

##################
##  ~ Set up ~  ##
##################
# SET WORKING DIRECTORY # #####
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/streamflow_data")
Today <- format(Sys.Date(),"%Y%m%d")

#####
# LOAD REQUIRED PACKAGES # #####
require(ggplot2)
require(mgcv)
require(hydromad)
#####
# STATIONS # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", 
                        "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502",
                        "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", 
                        "YARR"="614044", "DOMB"="607155")

# read in the data
load("C:/Users/rver4657/ownCloud/Virtual Experiments/ProjectData/20150925_ClimCh_project_MD.Rdata")


# analyse trends in CV and trends in Q10 and Q90
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

windows(height=10,width=20)
ggplot(plot.df,aes(x=Date,y=data)) + geom_line() + facet_wrap(~station)
# No clear indication of a trend

# Calculate averages 
annualT <- aggregate(maxT_zoo,format(time(rain_zoo),"%Y"),mean,na.rm=T)
mean_annualT <- apply(annualT,2,mean,na.rm=T)

# what about variation in CV
CV_annualF <- aggregate(flow_zoo,format(time(flow_zoo),"%Y"),
                        function(x) sd(x,na.rm=T)/mean(x,na.rm=T))

plot(CV_annualF)
CV_monthF <- aggregate(flow_zoo,as.yearmon,
                       function(x) sd(x,na.rm=T)/mean(x,na.rm=T))
# Check cumulative running anomalies
plot(CV_monthF)

# Now analyse this for trends?
# Visually no real trends
# Calculate monthly precip and maxT
monthR <- aggregate(rain_zoo,as.yearmon, mean,na.rm=T)
monthT <- aggregate(maxT_zoo,as.yearmon, mean,na.rm=T)

FDC_results <- list()


# calculate the flow duration curve and the Q10 and Q90
# save only the Q10 and Q90 series
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_fdc", sep=""), 
         quantile(flow_zoo[,i], 
                  probs=c(0.2,0.8,0.9,0.95)))
  FDC_results[[i]] <- data.frame(station = colnames(flow_stns[i]),
                                    Date=time(flow_zoo),
                                 Q20 = replace(flow_zoo[,i],flow_zoo[,i] > 
                                   get(paste(colnames(flow_stns[i]), "_fdc", sep=""))[1],NA),
                                 Q80 = replace(flow_zoo[,i],flow_zoo[,i] < 
                                   get(paste(colnames(flow_stns[i]), "_fdc", sep=""))[2],NA),
                                 Q90 = replace(flow_zoo[,i],flow_zoo[,i] < 
                                      get(paste(colnames(flow_stns[i]), "_fdc", sep=""))[3],NA),
                                 Q95 = replace(flow_zoo[,i],flow_zoo[,i] < 
                                      get(paste(colnames(flow_stns[i]), "_fdc", sep=""))[4],NA))
  print(i)
}

# combine into a dataframe
FDC_df <- do.call(rbind,FDC_results)
# check
#p <- ggplot(FDC_df,aes(x=Date,y=as.numeric(Q90))) + geom_line() + facet_wrap(~station)
#p

# Now analyse trends in the Q10 and Q90 series
# Q95
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(FDC_df, station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  #  gamm.data$lagRain <- lag(gamm.data$Rain)
  #  gamm.data <- na.omit(gamm.data)
  #  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("fdc95_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gls(Q95~trend, 
             corr = corCAR1(), data=gamm.data, na.action=na.exclude))
  print(i)
}

gamm.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list[[i]] <- get(paste("fdc_gam.temp.", 
                              colnames(flow_stns[i]), sep=""))
}

# look at results
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  print(summary(gamm.list[[i]])$tTable)
}

# now run Q20
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(FDC_df, station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  #  gamm.data$lagRain <- lag(gamm.data$Rain)
  #  gamm.data <- na.omit(gamm.data)
  #  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("fdcQ20_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gls(log(Q20+1)~trend, corr = corCAR1(), 
             data=gamm.data, na.action=na.exclude))
  print(i)
}

gammQ20.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gammQ20.list[[i]] <- get(paste("fdcQ20_gam.temp.", 
                        colnames(flow_stns[i]), sep=""))
}

# look at results
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  print(summary(gammQ20.list[[i]])$tTable)
}


# take into account trend in rain
# need to first add rainfall to the df
for (i in seq_along(flow_stns)) {
  gamm.data <- subset(FDC_df, station==colnames(flow_stns[i]))
  gamm.data$trend <- 1:nrow(gamm.data)
  #  gamm.data$lagRain <- lag(gamm.data$Rain)
  #  gamm.data <- na.omit(gamm.data)
  #  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("fdc95P_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gamm(log(Q95)~trend + s(P), 
             corr = corCAR1(), data=gamm.data, na.action=na.exclude))
  print(i)
}

gamm.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list[[i]] <- get(paste("fdc_gam.temp.", colnames(flow_stns[i]), sep=""))
}

# look at results
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  print(summary(gamm.list[[i]])$tTable)
}


for (i in seq_along(flow_stns)) {
  gamm.data <- subset(gam_an_df, stn==colnames(flow_stns[i]))
  #  gamm.data$lagRain <- lag(gamm.data$Rain)
  #  gamm.data <- na.omit(gamm.data)
  #  gamm.data$lagFlow <- lag(gamm.data$Flow)
  assign(paste("flow_an_gam.temp.", colnames(flow_stns[i]), sep=""), 
         gls(rain~trend, 
             corr = corCAR1(), data=gamm.data))
  print(i)
}

gamm.list <- vector("list", length=length(seq_along(flow_stns)))
for (i in seq_along(flow_stns)) {
  gamm.list[[i]] <- get(paste("flow_an_gam.temp.", colnames(flow_stns[i]), sep=""))
}

# look at results
for (i in seq_along(flow_stns)) {
  print(colnames(flow_stns[i]))
  print(summary(gamm.list[[i]])$tTable)
}
