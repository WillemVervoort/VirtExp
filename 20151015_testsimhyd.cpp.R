
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015")

require(Rcpp)

sourceCpp("20161014_Simhyd.cpp")

require(hydromad)
data(Cotter)
head(Cotter)



test <- simhyd_sim(Cotter$P[1:1000], 0.15*Cotter$E[1:1000],
                   INSC = 0.6, COEFF = 200, SQ = 1.5,SMSC = 500, 
                   SUB = 1,CRAK = 0.08,K = 0.3)

