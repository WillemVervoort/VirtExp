#################################
### CLIMATE CHANGE STREAM FLOW ##
#################################
# look at differences between applying scaling to weekly or monthly data

setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")

require(zoo)
require(xts)
require(ggplot2)
Today <- format(Sys.Date(),"%Y%m%d")

load("ProjectData/20160804_ClimCh_project_MD.Rdata")

# create monthly data and annual data for all three
flow_zoo_m <- aggregate(flow_zoo,as.yearmon,sum,na.rm=T)
rain_zoo_m <- aggregate(rain_zoo,as.yearmon,sum,na.rm=T)
maxT_zoo_m <- aggregate(maxT_zoo,as.yearmon,mean,na.rm=T)

# annual data
annualF <- aggregate(flow_zoo,format(time(flow_zoo),"%Y"),sum,na.rm=T)
annualR <- aggregate(rain_zoo,format(time(rain_zoo),"%Y"),sum,na.rm=T)
annualmaxT <- aggregate(maxT_zoo,format(time(maxT_zoo),"%Y"),mean,na.rm=T)


mu <- cbind(rep(c(-15,-10,0,10),3),c(rep(0,4),rep(5,4),rep(10,4)))
# Create the precipitation and ET data variations
test <- list()
clim.adj <- list()
for (i in 1:ncol(rain_zoo)) {
  for (k in 1:nrow(mu)) {
    test[[k]] <- data.frame(rain=(1+mu[k,1]/100)*rain_zoo[,i],
                            temp=(1+mu[k,2]/100)*maxT_zoo[,i],
                            mu_rain=mu[k,1],mu_temp=mu[k,2],
                            station = names(flow_zoo)[i])
  }
  clim.adj[[i]] <- do.call(rbind,test)
}
# monthly data
test <- list()
clim.adj_m <- list()
for (i in 1:ncol(rain_zoo)) {
  for (k in 1:nrow(mu)) {
    test[[k]] <- data.frame(rain=(1+mu[k,1]/100)*rain_zoo_m[,i],
                            temp=(1+mu[k,2]/100)*maxT_zoo_m[,i],
                            mu_rain=mu[k,1],mu_temp=mu[k,2],
                            station = names(flow_zoo)[i])
  }
  clim.adj_m[[i]] <- do.call(rbind,test)
}

hist(clim.adj[[1]][,"rain"])
hist(clim.adj_m[[1]][,"rain"])

# extract only -10
min10 <- clim.adj[[1]][clim.adj[[1]]$mu_rain==-10,"rain"]
min10.z <- zoo(min10,order.by=time(rain_zoo))
m_min10.z <- aggregate(min10.z,as.yearmon,sum)

par(mfrow=c(2,1))
hist(m_min10.z,main="monthly aggregated adjusted weekly values",
     xlab="rainfall * -10%")
hist(clim.adj_m[[1]][clim.adj_m[[1]]$mu_rain==-10,"rain"],
     main="adjusted monthly values", xlab="rainfall * -10%")
par(mfrow=c(1,1))

month_min10 <- clim.adj_m[[1]][clim.adj_m[[1]]$mu_rain==-10,"rain"]

# show qqplot
qqplot(m_min10.z, month_min10,
       xlab="monthly aggregated adjusted weekly values",
       ylab="adjusted monthly values",
       main="qqplot of different scaling methods")



