setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/VirtExp")

load(file="data/GAMMmodel5Resid_grid.rdata")

require(tidyverse)
require(zoo)
require(Kendall)
library(xts)
library(doParallel)
library(foreach)
# Bootstrap
# now run a loop over the number of years (create 41 different sets)
# do Mann Kendall test on each resonstituted series
# ---------------------------
#  -------------------------
resid_temp <- as.data.frame(resid_df)
#resid_temp$date <- time(resid_df)
resid_temp$years <- format(time(resid_df),"%Y")
split_resid <- split(resid_temp[,1:13],resid_temp$years)


cl <- makeCluster(2) # create a cluster with 6 cores
registerDoParallel(cl) # register the cluster
# use a foreach loop to calibrate
MK_list <- foreach(i = 1:500,
                   .packages=c("Kendall","xts")) %dopar% {
                     # reorganise the list elements
                     series <- sample(1:nyears(resid_df),nyears(resid_df))
                     for (j in 1:length(series)) {
                       if (j==1) {
                         new_df <- as.data.frame(split_resid[[series[j]]])
                       } else {
                         new_df <- rbind(new_df,as.data.frame(split_resid[[series[j]]]))
                       }
                     }
                     # run mann kendall on the columns and store the results
                     mk_r <- apply(new_df,2,MannKendall)
                     
                     out <- do.call(cbind,mk_r)
                     out
                   }
stopCluster(cl)

MK_df <- do.call(rbind,MK_list)

pvalues <- subset(MK_df, rownames(MK_df)=="sl")
tau <- subset(MK_df, rownames(MK_df)=="tau")

sig_set <- list()

for (i in 1:ncol(pvalues)) {
  set <- data.frame(pvalue=as.numeric(pvalues[,i]),
                    tau=as.numeric(tau[,i]),catch=rep(colnames(MK_df)[i],nrow(tau)))
  sig_set[[i]] <- set[set$pvalue < 0.5,]
}

sig_set_a <- do.call(rbind,sig_set)
sig_set_a$type <- rep("bootstrap",nrow(sig_set_a))



MK_resid <- do.call(rbind,apply(resid_df,2,MannKendall))

real_df <- data.frame(pvalue = as.numeric(MK_resid[,2]),
                      tau = as.numeric(MK_resid[,1]),
                      catch=rownames(MK_resid),
                      type=rep("real",nrow(MK_resid)))


# A histogram of taus

hp <- ggplot(sig_set_a, aes(x=tau)) + 
  geom_histogram(binwidth=0.005,fill="blue", alpha=0.5)
# Histogram of significant tau's, divided by catch
# With panels that have the same scaling, but different range
# (and therefore different physical sizes)
hp <- hp + facet_wrap(~ catch,ncol=5) + theme_bw()
# add a red point for the real slope from the data
p_value <- ifelse(real_df$pvalue<0.05,"< 0.05",">= 0.05")
hp <- hp + geom_point(data=real_df,aes(x=tau, y=0,colour=p_value),
                      shape=16,size=5) +
  facet_wrap(~ catch,ncol=5) #+ ggtitle("Residuals Streamflow after GAM") #+
hp <- hp + scale_colour_manual(values = c("< 0.05" = "red", 
                                          ">= 0.05" = "blue"))
hp <- hp + theme(axis.text.x = element_text(angle=45,
                                            vjust = 0.5,
                                            hjust = 0.5, 
                                            size = rel(1.2)),
                 axis.text.y = element_text(size = rel(1.2)),
                 axis.title = element_text(size=rel(1.5)),
                 strip.text = element_text(size=rel(1.5), 
                                           face = "bold"))
print(hp)


tiff("../manuscript/Figure7_ResidGAM_MDPaper.tif",res=600,compression="lzw",
     width=10*480,height=10*480)
print(hp)
dev.off()
