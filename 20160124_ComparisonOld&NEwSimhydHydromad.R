# Simhyd implementation in hydromad
setwd("C:/Users/rver4657/ownCloud/Virtual Experiments/hpc/MD_ProjectRCode")

source("20151015_Simhyd.r")
# this version includes the cpp implementation, should be much faster

# testing SimHyd for hydromad
require(hydromad)
data(Cotter)

# fix some of the parameters
testSH <- hydromad(Cotter[1:5000,],sma="simhyd", routing="simhydrouting",
                   COEFF=200, SQ=1.5, 
                   etmult=0.15, K=0.3)
print(testSH)
# simulation
#mod1 <- simulate(testSH, 1, sampletype ="random")

# Trying to fit
testSH.fit <- fitByOptim(testSH,method="PORT",samples=100)
summary(testSH.fit)
# show fitted model
xyplot(testSH.fit)

# store with different name
testSH.fitOld <- testSH.fit
# compare to newer code
load("C:/Users/rver4657/ownCloud/Virtual Experiments/For Willem_05_06_2015/TestNewHydromad.Rdata")

summary(testSH.fit)
summary(testSH.fitOld)
# only minor differences

# prediction with a fully specified model also works
FitQ <- predict(testSH.fit, return_state = TRUE)
xyplot(window(cbind(Cotter[1:5000,1:2], simhyd = FitQ)))
