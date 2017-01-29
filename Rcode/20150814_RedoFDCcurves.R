####################################################
### CLIMATE CHANGE STREAM FLOW                    ##
### ~ Michaela Dolk and Willem Vervoort ~         ##
####################################################

# redo the FDC figures

# IMPORT FLOW DATA INTO R # #####
#setwd("/Users/michaeladolk/Desktop/Streamflow_data")
#setwd("U:/My-Workspace/mdol7996/Streamflow_2015")
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/streamflow_data")


flow_stns <- data.frame("COTT"="410730", "RUTH"="219001", "CORA"="215004", "ELIZ"="G8150018", "COCH"="113004A", "COEN"="922101B", "SCOT"="A5030502", "HELL"="312061", "NIVE"="304497", "MURR"="405205", "SOUT"="225020A", "YARR"="614044", "DOMB"="607155")
for (i in seq_along(flow_stns)) {
  assign(paste(colnames(flow_stns[i]), "_daily_flow", sep=""), read.csv(paste(flow_stns[,i],"_daily_ts2.csv", sep=""), col.names=c("Date", "Q")))
}
#####

# SELECT STUDY PERIOD # #####
# Note: could possibly merge this section with next section
flow_df_list <- list(COTT=COTT_daily_flow, RUTH=RUTH_daily_flow, CORA=CORA_daily_flow, ELIZ=ELIZ_daily_flow, COCH=COCH_daily_flow, COEN=COEN_daily_flow, SCOT=SCOT_daily_flow, HELL=HELL_daily_flow, NIVE=NIVE_daily_flow, MURR=MURR_daily_flow, SOUT=SOUT_daily_flow, YARR=YARR_daily_flow, DOMB=DOMB_daily_flow)
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")
subset_function <- function(x) subset(x, as.Date(Date, format="%d/%m/%y")>=start_date & as.Date(Date, format="%d/%m/%y")<=end_date)
flow_df_list <- lapply(flow_df_list, subset_function)
flow_data_70_10 <- data.frame(Date=flow_df_list$COTT$Date, COTT=flow_df_list$COTT$Q,
                              RUTH=flow_df_list$RUTH$Q, CORA=flow_df_list$CORA$Q,
                              ELIZ=flow_df_list$ELIZ$Q, COCH=flow_df_list$COCH$Q,
                              COEN=flow_df_list$COEN$Q, SCOT=flow_df_list$SCOT$Q,
                              HELL=flow_df_list$HELL$Q, NIVE=flow_df_list$NIVE$Q,
                              MURR=flow_df_list$MURR$Q, SOUT=flow_df_list$SOUT$Q,
                              YARR=flow_df_list$YARR$Q, DOMB=flow_df_list$DOMB$Q)
#####

# SUBSET DATA BY DECADE # #####
study_period_decades <- c("70_80", "80_90", "90_00", "00_10")
decade_start <- c(as.Date("1/1/1970", format="%d/%m/%Y"), as.Date("1/1/1980", format="%d/%m/%Y"), as.Date("1/1/1990", format="%d/%m/%Y"), as.Date("1/1/2000", format="%d/%m/%Y"))
decade_end <- c(as.Date("31/12/1979", format="%d/%m/%Y"), as.Date("31/12/1989", format="%d/%m/%Y"), as.Date("31/12/1999", format="%d/%m/%Y"), as.Date("31/12/2009", format="%d/%m/%Y"))
for (i in seq_along(study_period_decades)) {assign(paste("flow.", study_period_decades[i], sep=""), subset(flow_data_70_10, as.Date(Date, format="%d/%m/%y")>=decade_start[i] & as.Date(Date, format="%d/%m/%y")<=decade_end[i]))}
#####

# FLOW DURATION CURVES # #####
for (i in seq_along(study_period_decades)) {
  temp_n <- length(seq.Date(decade_start[i], decade_end[i], by="day"))
  temp_rank <- 1:temp_n
  assign(paste("prob.", study_period_decades[i], sep=""), temp_rank/(temp_n+1))
}
sort_function <- function(x) sort(x, decreasing=TRUE)
sort.70_80 <- data.frame(apply(flow.70_80[2:14],2, sort_function)) # might not need to put data.frame in front of this
sort.80_90 <- data.frame(apply(flow.80_90[2:14],2, sort_function))
sort.90_00 <- data.frame(apply(flow.90_00[2:14],2, sort_function))
sort.00_10 <- data.frame(apply(flow.00_10[2:14],2, sort_function))
# Plot
par(mfrow = c(3,5))
for (i in seq_along(sort.70_80)) {
  plot(prob.70_80*100, sort.70_80[,i], xlab="Probability of excedence %", ylab=paste(colnames(sort.70_80)[i], " Flow (ML/day)"), type="l", log="y", col="red")
  lines(prob.80_90*100, sort.80_90[,i], col="purple")
  lines(prob.90_00*100, sort.90_00[,i], col="blue")
  lines(prob.00_10*100, sort.00_10[,i], col="green")
}
plot(1, type="n", axes=F, xlab="", ylab="")
legend("center",legend=c("1970-1979","1980-1989","1990-1999","2000-2009"),lty=c(1,1),col=c("red","purple","blue","green"), cex=0.5, lwd=1, text.width=0.5)
par(mfrow = c(1,1))
#####

# DIFFERENCE FLOW DURATION CURVES # #####
# List of probabilities as percentages
Prob <- sort(unique(c(prob.70_80*100, prob.80_90*100, prob.90_00*100, prob.00_10*100)))
# Interpolate FDCs and difference
par(mfrow = c(3,5))
for (i in seq_along(sort.70_80)) {
  temp.FDC.70_80 <- data.frame(approx(prob.70_80*100, log(sort.70_80[,i]), Prob))
  temp.FDC.80_90 <- data.frame(approx(prob.80_90*100, log(sort.80_90[,i]), Prob))
  temp.FDC.90_00 <- data.frame(approx(prob.90_00*100, log(sort.90_00[,i]), Prob))
  temp.FDC.00_10 <- data.frame(approx(prob.00_10*100, log(sort.00_10[,i]), Prob))
  temp.FDC.diff.80_90 <- na.omit(data.frame(Prob, Diff=temp.FDC.70_80$y-temp.FDC.80_90$y))
  temp.FDC.diff.90_00 <- na.omit(data.frame(Prob, Diff=temp.FDC.70_80$y-temp.FDC.90_00$y))
  temp.FDC.diff.00_10 <- na.omit(data.frame(Prob, Diff=temp.FDC.70_80$y-temp.FDC.00_10$y))
  plot(temp.FDC.diff.80_90, type="l", col="purple", main=paste(colnames(sort.70_80)[i]))
  lines(temp.FDC.diff.90_00, col="blue")
  lines(temp.FDC.diff.00_10, col="green")
}
plot(1, type="n", axes=F, xlab="", ylab="")
legend("center",legend=c("(1970-1979) - (1980-1989)","(1970-1979) - (1990-1999)","(1970-1979) - (2000-2009)"),border="black",lty=c(1,1),col=c("purple","blue","green"), cex=0.5, lwd=1, text.width=0.5)
par(mfrow = c(1,1))
#####
