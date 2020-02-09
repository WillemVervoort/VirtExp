# Download ANUCLIM for the stations and 
# check whether any better for SOUTH, HELL and NIVE

setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/VirtExp")
require(curl)
require(tidyverse)
require(lubridate)
require(zoo)

Stations <- read.csv("Data/CatchmentCharact.csv")

#head(Stations)
part1 <- "http://dapds00.nci.org.au/thredds/ncss/rr9/eMAST_data/ANUClimate/ANUClimate_v1-0_rainfall_daily_0-01deg_1970-2014?var=lwe_thickness_of_precipitation_amount&latitude="
part2 <- "&time_start=1970-01-01T00%3A00%3A00Z&time_end=2010-12-31T00%3A00%3A000&accept=csv_file"

for (i in 1:nrow(Stations)) {
  url <- paste(part1,Stations$Latitude[i],"&longitude=",
               Stations$Longitude[i],part2, sep="")
  
  curl_download(url,destfile=paste("data/",Stations[i,1],
                                   "_ANUclimRain.csv",sep=""))
  
}

# load in the existing data
load("data/ClimCh_project_MD.Rdata")

# combine ANUclim data together in a data frame similar to flow_zoo
for (i in 1:nrow(Stations)) {
    temp<-read.csv(paste("data/",Stations[i,1],
                         "_ANUclimRain.csv",sep=""))
    temp$time<-ymd(substr(temp$time,1,10))
    temp <- temp %>%
      select(time, `lwe_thickness_of_precipitation_amount.unit.mm.day.1.`)
    colnames(temp) <- c("Date", "Rainfall")
    assign(paste(Stations[i,1], "RainAC", sep=""), 
           zoo(temp$Rainfall, order.by=temp$Date))
  }
  # merge 
  rainAC_zoo<-merge(COTTRainAC, RUTHRainAC, CORARainAC, ELIZRainAC, COCHRainAC, 
                  COENRainAC, SCOTRainAC, HELLRainAC, NIVERainAC, MURRRainAC, 
                  SOUTRainAC, YARRRainAC, DOMBRainAC)  
  
# annual sums  
rainAC_zoo_t <- as.tibble(rainAC_zoo) %>%
  gather(key = "Station", value="Rain", COTTRainAC:DOMBRainAC) %>%
  group_by(year=rep(year(time(rainAC_zoo)),13), Station = Station) %>%
  summarise(Rain = sum(Rain)) %>%
  mutate(Station = substr(Station,1,4))
# annual sums flow
flow_zoo_t <- as.tibble(flow_zoo) %>%
  gather(key = "Station", value="Flow", COTT_daily_flow:DOMB_daily_flow) %>%
  group_by(year=rep(year(time(flow_zoo)),13), Station = Station) %>%
  summarise(Flow = sum(Flow, na.rm=T)) %>%
  mutate(Station = substr(Station,1,4))

plot_df <- full_join(rainAC_zoo_t,flow_zoo_t)

plot_df %>%
  ggplot(aes(Rain,Flow)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype =2) +
  facet_wrap(~Station)
