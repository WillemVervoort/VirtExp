require(tidyverse)

flow_stns <- c("COTT", "RUTH", "CORA", "ELIZ", "COCH", "COEN", "SCOT", "HELL", "NIVE", "MURR", "SOUT", "YARR", "DOMB")


# read in the GR4J st data from the HPC
files <- dir(path="Projectdata/HPCresults/",pattern="GR4J_st")

GR4J_st_Out <- foreach(i=1:length(files), .combine='rbind') %do%
{
  load(paste("Projectdata/HPCresults/",files[i], sep=""))
  k <- ifelse(i < 7,20,10)
  names(Station_resid) <- c(rep("Mann_Kendall",k),
                                 rep("Significance_of_H",k),
                                 rep("Mann_Kendall_LTP",k))
  MK_standard <- as_tibble(do.call(rbind,
                          Station_resid[names(Station_resid)=="Mann_Kendall"]))
  Hsignif <- as_tibble(do.call(rbind,
                        Station_resid[names(Station_resid)=="Significance_of_H"]))
  names(Hsignif)[2] <- "2_sided_pvalue_Hest"
  MK_LTP <- as_tibble(do.call(rbind,
                        Station_resid[names(Station_resid)=="Mann_Kendall_LTP"]))
  names(MK_LTP)[2] <- "2_sided_pvalue_MKLTP"
       
  flow_MKLTP <- cbind(MK_standard, Hsignif, MK_LTP)
       
  if (i < 7) {
    st_names <- rep(flow_stns[((i-1)*2+1):(i*2)],each=10)
  } else {
    st_names <- rep(flow_stns[(i-1)*2+1],10)  
  }
                         
       
  flow_MKLTP <- as_tibble(flow_MKLTP) %>%
         mutate(Station = st_names) %>%
         group_by(Station) %>%
         summarise(MK_tau = mean(Kendall_s_tau_statistic),
                   var_MK_tau = var(Kendall_s_tau_statistic),
                   p_MK = mean(`2_sided_pvalue`),
                   p_MKLTP = mean(`2_sided_pvalue_MKLTP`))
  flow_MKLTP
       
}

# read in the GR4J gr data from the HPC
files <- dir(path="Projectdata/HPCresults/",pattern="GR4J_gr")

GR4J_gr_Out <- foreach(i=1:length(files), .combine='rbind') %do%
{
  load(paste("Projectdata/HPCresults/",files[i], sep=""))
  k <- ifelse(i < 7,20,10)
  names(Station_resid) <- c(rep("Mann_Kendall",k),
                            rep("Significance_of_H",k),
                            rep("Mann_Kendall_LTP",k))
  MK_standard <- as_tibble(do.call(rbind,
                                   Station_resid[names(Station_resid)=="Mann_Kendall"]))
  Hsignif <- as_tibble(do.call(rbind,
                               Station_resid[names(Station_resid)=="Significance_of_H"]))
  names(Hsignif)[2] <- "2_sided_pvalue_Hest"
  MK_LTP <- as_tibble(do.call(rbind,
                              Station_resid[names(Station_resid)=="Mann_Kendall_LTP"]))
  names(MK_LTP)[2] <- "2_sided_pvalue_MKLTP"
  
  flow_MKLTP <- cbind(MK_standard, Hsignif, MK_LTP)
  
  if (i < 7) {
    st_names <- rep(flow_stns[((i-1)*2+1):(i*2)],each=10)
  } else {
    st_names <- rep(flow_stns[(i-1)*2+1],10)  
  }
  
  
  flow_MKLTP <- as_tibble(flow_MKLTP) %>%
    mutate(Station = st_names) %>%
    group_by(Station) %>%
    summarise(MK_tau = mean(Kendall_s_tau_statistic),
              var_MK_tau = var(Kendall_s_tau_statistic),
              p_MK = mean(`2_sided_pvalue`),
              p_MKLTP = mean(`2_sided_pvalue_MKLTP`))
  flow_MKLTP
  
}
