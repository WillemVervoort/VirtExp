## hydromad: Hydrological Modelling and Analysis of Data
## Rewrite
## willem 20151014
## Coded based on diagram and description in:
# Chiew et al 2009 WATER RESOURCES RESEARCH, VOL. 45, W10414, doi:10.1029/2008WR007338, 2009
require(Rcpp)

sourceCpp("20151014_Simhyd.cpp")


## SimHyd model
simhyd.sim <-
    function(DATA,
             INSC,COEFF,
             SQ,
             SMSC, SUB, CRAK, K, 
             etmult = 0.15,
             return_state = FALSE)

	# See Figure 2 in Chiew et al. 2009
  # INSC interception store capacity (mm)
  # COEFF maximum infiltration loss
  # SQ Infiltration loss exponent
  #  SMSC = Soil Moisture Storage Capacity
  # SUB constant of proportionality in interflow equation
  # CRAK constant of proportionality in groundwater rechareg equation
  # K baseflow linear recession parameter    
	# etmult = added parameter to convert maxT to PET

{
    stopifnot(c("P","E") %in% colnames(DATA))
    ## check values
    stopifnot(INSC >= 0)
    stopifnot(COEFF >= 0)
    stopifnot(SQ >= 0)
    stopifnot(SMSC >= 0)
    stopifnot(SUB >= 0)
    stopifnot(CRAK >= 0)
    stopifnot(K >= 0)

    xpar <-
        c(INSC, COEFF, SQ, SMSC, SUB, CRAK, K)
 
    inAttr <- attributes(DATA[,1])
    DATA <- as.ts(DATA)

    P <- DATA[,"P"]
    E <- etmult*DATA[,"E"]
    ## skip over missing values
    bad <- is.na(P) | is.na(E)
    P[bad] <- 0
    E[bad] <- 0

    COMPILED <- (hydromad.getOption("pure.R.code") == FALSE)
    if (COMPILED) {
    # run the cpp version
      ans <- simhyd_sim(P, E, INSC, COEFF, SQ,SMSC, 
                 SUB,CRAK,K)
      U <- ans$U
      aET <- ans$ET
    } else {	 ## very slow, even on my x64
    U <- IMAX <- INT <- INR <- RMO <- IRUN <- P
    aET <- ET <- SRUN <- REC <- SMF <- POT <- BAS <- P
	  SMS <- GW <- rep(0,length(P))
    SMS[1] <- 0.5*SMSC
# run through a loop
    for (t in seq(2, length(P))) {
## testing
#t <- 2
  		# interception store
  		IMAX[t] <- min(INSC,E[t])
  		#print(IMAX[t])
  		# calculate interception
  		INT[t] <- min(IMAX[t],P[t])
  		#print(INT[t])
  		# calculate interception runoff (INR)
  		INR[t] <- P[t] - INT[t]
  		#print(INR[t])
  		# Calculate infiltration capacity
  		RMO[t] <- min(COEFF*exp(-SQ*SMS[t-1]/SMSC),INR[t])
  		#print(RMO[t])
      # calculate direct runoff
  		IRUN[t] <- INR[t] - RMO[t]
  		#print(IRUN[t])
      # SRUN (Saturation excess runoff and interflow)
  		SRUN[t] = SUB*SMS[t-1]/SMSC*RMO[t]
  		#print(SRUN[t])
  		# calculate Recharge
  		REC[t] <- CRAK*SMS[t-1]/SMSC*(RMO[t] - SRUN[t])
  		#print(REC[t])
  		# INfiltration into soil store (SMF)
  		SMF[t] <- RMO[t] - SRUN[t] - REC[t]
  		#print(SMF[t])
  		# calculate potential ET
  		POT[t] <- E[t] - INT[t]
  		# Calculate Soil ET
  		ET[t] <- min(10*SMS[t-1]/SMSC,POT[t])
  		#print(ET[t])
  		# calculate SMS overflow (see Figure 2 in Chiew et al 2009)
  		# calculate soil moisture storage
  		SMS[t] <- SMS[t-1] + SMF[t] - ET[t]
  		if (SMS[t] > SMSC) {
  		  SMS[t] <- SMSC
  		  REC[t] <- REC[t] + SMS[t] - SMSC
  		}
  		#print(SMS[t])
  		# calculate baseflow
  		BAS[t] <- K*GW[t-1]
  		#print(BAS[t])
  		# Calculate GW storage
  		GW[t] <- GW[t-1] + REC[t] - BAS[t]
  		#print(GW[t])
      # Calculate runoff
  		U[t] <- IRUN[t] + SRUN[t] + BAS[t]
  		#print(paste("U =",U[t]))
  		aET[t] <- ET[t]
    }
  }
  ## make it a time series object again
  attributes(U) <- inAttr
  attributes(aET) <- inAttr
  ## re-insert missing values
  U[bad] <- NA
  aET[bad] <- NA
  if (return_state==TRUE) {
  return(merge(U=U,aET=aET))
  } else {
     return(U)
  }
}

# Routing based on Muskinghum
simhydrouting.sim <- function(U, DELAY=0, X_m=0, return_components = FALSE) {
  X <- rep(0,length(U))
  inAttr <- attributes(U)
  U <- as.ts(U)
  bad <- is.na(U)
  U[bad] <- 1
    # Muskingum components
  C0 <- (-DELAY*X_m+0.5)/(DELAY*(1-X_m)+0.5)
  #print(C0)
  C1 <- (DELAY*X_m+0.5)/(DELAY*(1-X_m)+0.5)
  #print(C1)
  C2 <- (DELAY*(1-X_m)-0.5)/(DELAY*(1-X_m)+0.5)
  #print(C2)
  
  X[1] <- U[1]
  for(t in 1:(length(U)-1)){ 
    X[t+1] <- C0*U[t+1]+C1*U[t]+C2*X[t]
    #print(X[t+1])
  }
  attributes(X) <- inAttr
  X[bad] <- NA
  return(X)
}   

simhyd.ranges <- function() 
    list(INSC = c(0,50),
         COEFF = c(0.0,400),
         SQ = c(0,10), 
         SMSC = c(1,1000),
         SUB = c(0.0,1),
         CRAK = c(0.0,1),
         K = c(0.0,1),
        etmult = c(0,1),
        DELAY = c(0,100),
        X_m = c(0,0.5))

# testing
require(hydromad)
data(Cotter)

# fix some of the parameters
testSH <- hydromad(Cotter[1:1000,],sma="simhyd", routing="simhydrouting",
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

# prediction with a fully specified model also works
FitQ <- predict(testSH.fit, return_state = TRUE)
xyplot(window(cbind(Cotter[1:1000,1:2], simhyd = FitQ)))


