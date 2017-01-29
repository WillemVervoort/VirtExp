# redo the FDC figures
Today <- format(Sys.Date(),"%Y%m%d")
# IMPORT FLOW DATA INTO R # #####
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments")


# LOAD REQUIRED PACKAGES # #####
library(xts)
library(zoo)
library(ggplot2)
#####
# STATIONS # #####
#####
# FLOW, RAINFALL AND TEMPERATURE DATA # #####
flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
# Flow data
# read in catchment characteristics to get sizes

load("ProjectData/20160726_ClimCh_project_MD.Rdata")

#head(flow_rain_maxT_weekly)

# Reviewer's question about trends, do a simple regression analysis
# visualise this using ggplot
p <- ggplot(flow_rain_maxT_weekly, aes(x=Date, y=Flow)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)
p <- p + facet_wrap(~Station)
p

# now do the official lm analysis
# create storage for results
lm_data <- data.frame(Date=flow_rain_maxT_weekly$Date,
                      Flow=flow_rain_maxT_weekly$Flow,
                      Station=flow_rain_maxT_weekly$Station)
lm_results <- list()
stn <- unique(lm_data$Station)
for (i in 1:length(stn)) {
  sub_data <- subset(lm_data,lm_data$Station == stn[i])
  lm_results[[i]] <- summary(lm(sub_data$Flow~sub_data$Date))
}

# extract estimates and p-values
store <- data.frame(slope=rep(0,length(stn)), pval=rep(0,length(stn)))
for (j in 1:length(stn)) {
  store$slope[j] <- lm_results[[j]]$coefficients[2,1]*52
  store$pval[j] <- lm_results[[j]]$coefficients[2,4]
}
store
