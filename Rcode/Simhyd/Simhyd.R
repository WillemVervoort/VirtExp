## hydromad: Hydrological Modelling and Analysis of Data
## Rewrite
## willem V
## Including code for four versions of Simhyd
# references
# eWater: Podger (2004) https://ewater.atlassian.net/wiki/display/SD41/SIMHYD+-+SRG
# Chiew et al. (2002) Chapter 11 in VP Singh (ed) Mathematical Models of Small Watershed Hydrology and Applications
# Water Resources Publication, 2002 - Technology & Engineering - 950 pages
# Chiew et al. (2009) WRR VOL. 45, W10414, doi:10.1029/2008WR007338, 2009
# Chiew 2006 Hydrological Sciences-Journal-des Sciences Hydrologiques, 51(4) 2006
# Mun Ju Shin's/Felix Andrew's version rewritten in R and cpp

# temporarily, until fully compiled
# compile the cpp code
#sourceCpp("/home/562/wxv562/MD_ProjectRCode/SimhydC2002.cpp") # Chiew et al. 2002
#sourceCpp("/home/562/wxv562/MD_ProjectRCode/SimhydC2009.cpp") # Chiew et al. 2009 and Chiew 2006
#sourceCpp("/home/562/wxv562/MD_ProjectRCode/Simhyd_eWater.cpp") # eWater and SOURCE version
#sourceCpp("/home/562/wxv562/MD_ProjectRCode/SimhydMJEq.cpp") # rewrite of Min Ju Shin's version

# sourceCpp("/g/data1/rr9/wxv562/MD_ProjectRCode/SimhydC2002.cpp") # Chiew et al. 2002
# sourceCpp("/g/data1/rr9/wxv562/MD_ProjectRCode/SimhydC2009.cpp") # Chiew et al. 2009 and Chiew 2006
# sourceCpp("/g/data1/rr9/wxv562/MD_ProjectRCode/Simhyd_eWater.cpp") # eWater and SOURCE version
# sourceCpp("/g/data1/rr9/wxv562/MD_ProjectRCode/SimhydMJEq.cpp") # rewrite of Min Ju Shin's version

sourceCpp(paste(rcode_dir,"SimhydC2002.cpp",sep="/")) # Chiew et al. 2002
sourceCpp(paste(rcode_dir,"SimhydC2009.cpp", sep="/")) # Chiew et al. 2009 and Chiew 2006
sourceCpp(paste(rcode_dir,"Simhyd_eWater.cpp",sep="/")) # eWater and SOURCE version
sourceCpp(paste(rcode_dir,"SimhydMJEq.cpp",sep="/")) # rewrite of Min Ju Shin's version



## SimHyd_C2009 model
simhyd_C2009.sim <-
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
  # return_state, whether or not to return all components

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
      ans <- simhydC2009_sim(P, E, INSC, COEFF, SQ,SMSC, 
                 SUB,CRAK,K)
      U <- ans$U
      aET <- ans$ET
      if (return_state==T) {
        INR = ans$INR
        INT = ans$INT
        RMO = ans$RMO
        IRUN = ans$IRUN
        SRUN = ans$SRUN
        RMO= ans$RMO
        SMF = ans$SMF
        SMS = ans$SMS
        REC = ans$REC
        GW = ans$GW
        ET = ans$ET
        BAS = ans$BAS
      }
    } else {	 ## very slow, even on my x64
    U <- IMAX <- INT <- INR <- RMO <- IRUN <- NA
    aET <- ET <- SRUN <- REC <- SMF <- POT <- BAS <- NA
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
  		# Infiltration into soil store (SMF)
  		SMF[t] <- RMO[t] - SRUN[t] - REC[t]
  		#print(SMF[t])
  		# calculate potential ET
  		POT[t] <- E[t] - INT[t]
  		#print(E[t])
  		# calculate SMS overflow (see Figure 2 in Chiew et al 2009)
  		# calculate soil moisture storage
  		SMS[t] <- SMS[t-1] + SMF[t]
  		#print(SMS[t])
  		if (SMS[t] > SMSC) {
  		  REC[t] <- REC[t] + SMS[t] - SMSC
  		  SMS[t] <- SMSC
  		}
  		# Calculate Soil ET
  		ET[t] <- min(10*SMS[t]/SMSC,POT[t])
  		SMS[t] <- SMS[t] - ET[t]
  		#print(SMS[t])
  		# Calculate GW storage
  		GW[t] <- GW[t-1] + REC[t] 
  		# calculate baseflow
  		BAS[t] <- K*GW[t-1]
  		GW[t] <- GW[t] - BAS[t]
      # Calculate runoff
  		U[t] <- IRUN[t] + SRUN[t] + BAS[t]
  		#print(paste("U =",U[t]))
  		aET[t] <- ET[t] + IMAX[t]
    }
  }
  ## missing values
    U[bad] <- NA
    if (return_state==T) {
      aET[bad] <- NA
      INT[bad] <- NA
      INR[bad] <- NA
      RMO[bad] <- NA
      IRUN[bad] <- NA
      REC[bad] <- NA
      SMF[bad] <- NA
      ET[bad] <- NA
      SMS[bad] <- NA
      BAS[bad] <- NA
      GW[bad] <- NA
    }
    ## attributes
    attributes(U) <- inAttr
    ans <- U
    if (return_state==T) {
        attributes(aET) <- inAttr
        attributes(INT) <- inAttr
        attributes(INR) <- inAttr
        attributes(RMO) <- inAttr
        attributes(IRUN) <- inAttr
        attributes(SRUN) <- inAttr
        attributes(REC) <- inAttr
        attributes(SMF) <- inAttr
        attributes(ET) <- inAttr
        attributes(SMS) <- inAttr
        attributes(BAS) <- inAttr
        attributes(GW) <- inAttr
       ans <- cbind(U=U,aET=aET, throughfall = INR,
                    interceptionET = INT,
                    infiltration = RMO,
                    infiltrationXSRunoff = IRUN,
                    interflowRunoff = SRUN,
                    infiltrationAfterInterflow = RMO-SRUN,
                    soilInput = SMF,
                    soilMoistureStore = SMS,
                    recharge = REC,
                    groundwater=GW,
                    soilET = ET,
                    baseflow = BAS)
    }
    ans
}

## SimHyd_C2002 model
simhyd_C2002.sim <-
  function(DATA,
           INSC,COEFF,
           SQ,
           SMSC, SUB, CRAK, K, 
           etmult = 0.15,
           return_state = FALSE)
    
    # See Figure on page 339 in Chiew et al. 2002
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
      ans <- simhydC2002_sim(P, E, INSC, COEFF, SQ,SMSC, 
                              SUB,CRAK,K)
      U <- ans$U
      aET <- ans$ET
      if (return_state==T) {
        INR = ans$INR
        INT = ans$INT
        RMO = ans$RMO
        IRUN = ans$IRUN
        SRUN = ans$SRUN
        RMO= ans$RMO
        SMF = ans$SMF
        SMS = ans$SMS
        REC = ans$REC
        GW = ans$GW
        ET = ans$ET
        BAS = ans$BAS
      }
    } else {	 ## very slow, even on my x64
      U <- INT <- INR <- RMO <- IRUN <- NA
      aET <- ET <- SRUN <- REC <- SMF <- BAS <- NA
      SMS <- GW <- rep(0,length(P))
      SMS[1] <- 0.5*SMSC
      # run through a loop
      for (t in seq(2, length(P))) {
        # interception store
        tempET <- min(INSC,E[t])
        # calculate interception
        INT[t] <- min(tempET,P[t])
        # calculate interception runoff (INR)
        INR[t] <- P[t] - INT[t]
        #print(INR[t])
        # Calculate infiltration
        RMO[t] <- min(COEFF*exp(-SQ*SMS[t-1]/SMSC),INR[t])
        #print(RMO[t])
        # calculate direct runoff
        IRUN[t] <- INR[t] - RMO[t]
        # SRUN (Saturation excess runoff and interflow)
        SRUN[t] = SUB*SMS[t-1]/SMSC*RMO[t]
        # calculate Recharge
        REC[t] <- CRAK*SMS[t-1]/SMSC*(RMO[t] - SRUN[t])
        # Infiltration into soil store (SMF)
        SMF[t] <- RMO[t] - SRUN[t] - REC[t]
        # calculate SMS overflow
        # calculate soil moisture storage
        SMS[t] <- SMS[t-1] + SMF[t]
        if (SMS[t] > SMSC) {
          REC[t] <- REC[t] + SMS[t] - SMSC
          SMS[t] <- SMSC
        }
        # Calculate Soil ET
        ET[t] <- min(10*SMS[t]/SMSC,E[t])
        SMS[t] <- SMS[t] - ET[t]
       # Calculate GW storage
        GW[t] <- GW[t-1] + REC[t] 
        # calculate baseflow
        BAS[t] <- K*GW[t-1]
		GW[t] <- GW[t] - BAS[t]
        # Calculate runoff
        U[t] <- IRUN[t] + SRUN[t] + BAS[t]
        aET[t] <- ET[t] + tempET
      }
    }
    ## make it a time series object again
    ## re-insert missing values
    U[bad] <- NA
    aET[bad] <- NA
    if (return_state==T) {
      INT[bad] <- NA
      INR[bad] <- NA
      RMO[bad] <- NA
      IRUN[bad] <- NA
      REC[bad] <- NA
      SMF[bad] <- NA
      ET[bad] <- NA
      SMS[bad] <- NA
      BAS[bad] <- NA
      GW[bad] <- NA
    }
      attributes(U) <- inAttr
      ans <- U
      if (return_state==T) {
        attributes(aET) <- inAttr
        attributes(INT) <- inAttr
        attributes(INR) <- inAttr
        attributes(RMO) <- inAttr
        attributes(IRUN) <- inAttr
        attributes(SRUN) <- inAttr
        attributes(REC) <- inAttr
        attributes(SMF) <- inAttr
        attributes(ET) <- inAttr
        attributes(SMS) <- inAttr
        attributes(BAS) <- inAttr
        attributes(GW) <- inAttr
        #browser()
        ans <- cbind(U=U,aET=aET, throughfall = INR,
                     interceptionET = INT,
                     infiltration = RMO,
                     infiltrationXSRunoff = IRUN,
                     interflowRunoff = SRUN,
                     infiltrationAfterInterflow = RMO-SRUN,
                     soilInput = SMF,
                     soilMoistureStore = SMS,
                     recharge = REC,
                     groundwater=GW,
                     soilET = ET,
                     baseflow = BAS)
    }
    ans
  }

## SimHyd_eWater model
simhyd_eWater.sim <-
  function(DATA,
           pFrac = 1, impTh = 1,
           INSC,COEFF,
           SQ,
           SMSC, SUB, CRAK, K, 
           etmult = 0.15,
           return_state = FALSE)
    
    # Same as Chiew et al. 2002 except for impervious
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
      c(pFrac, impTh, INSC, COEFF, SQ, SMSC, SUB, CRAK, K)
    
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
      ans <- simhydeWater_sim(P, E, pFrac, impTh,
                              INSC, COEFF, SQ,SMSC, 
                              SUB,CRAK,K)
      U <- ans$U
      if (return_state==T) {
        aET <- ans$ET
        INR = ans$INR
        INT = ans$INT
        INF = ans$INF
        RMO = ans$RMO
        IRUN = ans$IRUN
        SRUN = ans$SRUN
        RMO= ans$RMO
        SMF = ans$SMF
        SMS = ans$SMS
        REC = ans$REC
        GW = ans$GW
        ET = ans$ET
        BAS = ans$BAS
      }
    } else {	 ## very slow, even on my x64
      U <- INT <- INF <- INR <- RMO <- IRUN <- ImpQ <- NA
      aET <- ET <- ImpET <- SRUN <- REC <- SMF <- BAS <- NA
      SMS <- GW <- rep(0,length(P))
      SMS[1] <- 0.33*SMSC
      # run through a loop
      for (t in seq(2, length(P))) {
		# equation 1 (both these equations
        ImpET[t] = min(min(E[t],(1 - pFrac)*impTh),(1- pFrac)*P[t])
        # calculate impervious runoff as remainder
        ImpQ[t] = 0.0
        if((1-pFrac)*P[t] - ImpET[t]>0) {
          ImpQ[t] =  (1-pFrac)*P[t] - ImpET[t]
        }
        # interception store
        tempET <- min(INSC,E[t])
        # equation 2 calculate interception
        INT[t] <- min(tempET,pFRac*P[t])
        # calculate interception runoff (INR)
        INR[t] <- pFrac*P[t] - INT[t]
        #print(INR[t])
        # equation 3 Calculate infiltration capacity
        RMO[t] <- min(COEFF*exp(-SQ*SMS[t-1]/SMSC),INR[t])
        # calculate direct (infiltration excess) runoff
        IRUN[t] <- INR[t] - RMO[t]
        # equation 5 Interflow runoff 
        SRUN[t] = SUB*SMS[t-1]/SMSC*RMO[t]
		# equation 6 infiltration after interflow
		# (INF[t] - SRUN[t]
        # equation 7 calculate Recharge
        REC[t] <- CRAK*SMS[t-1]/SMSC*(RMO[t] - SRUN[t])
        # equantion 8 Infiltration into soil store (SMF)
        SMF[t] <- RMO[t] - SRUN[t] - REC[t]
        # calculate SMS overflow
        # calculate soil moisture storage
        SMS[t] <- SMS[t-1] + SMF[t]
        if (SMS[t] > SMSC) {
          REC[t] <- REC[t] + SMS[t] - SMSC
          SMS[t] <- SMSC
        }
        # Calculate Soil ET
        ET[t] <- min(10*SMS[t]/SMSC,E[t])
        SMS[t] <- SMS[t] - ET[t]
        # calculate GW balance
        GW[t] <- GW[t-1] + REC[t]
    		# calculate baseflow
		    BAS[t] <- K*GW[t]
        # Calculate GW storage
        GW[t] <- GW[t] - BAS[t]
        # Calculate runoff
        U[t] <- IRUN[t] + SRUN[t] + BAS[t]
        aET[t] <- ET[t] + tempET + ImpET
      }
    }
    ## re-insert missing values
    U[bad] <- NA
    aET[bad] <- NA
    if (return_state==T) {
      INT[bad] <- NA
      INR[bad] <- NA
      RMO[bad] <- NA
      IRUN[bad] <- NA
      REC[bad] <- NA
      SMF[bad] <- NA
      ET[bad] <- NA
      SMS[bad] <- NA
      BAS[bad] <- NA
      GW[bad] <- NA
      }
    ## make it a time series object again
    # output all the different waterbalance components
    attributes(U) <- inAttr
    ans <- U
      if (return_state==T) {
        attributes(aET) <- inAttr
        attributes(INT) <- inAttr
        attributes(INR) <- inAttr
        attributes(RMO) <- inAttr
        attributes(IRUN) <- inAttr
        attributes(SRUN) <- inAttr
        attributes(REC) <- inAttr
        attributes(SMF) <- inAttr
        attributes(ET) <- inAttr
        attributes(SMS) <- inAttr
        attributes(BAS) <- inAttr
        attributes(GW) <- inAttr
        ans <- cbind(U=U,aET=aET, throughfall = INR,
					interceptionET = INT,
					infiltration = RMO,
          infiltrationXSRunoff = IRUN,
          interflowRunoff = SRUN,
				   infiltrationAfterInterflow = RMO-SRUN,
				   soilInput = SMF,
				   soilMoistureStore = SMS,
				   recharge = REC,
 				   groundwater=GW,
				   soilET = ET,
           baseflow = BAS)
      }
  ans
}

# Mun-Ju/Felix Andrews equivalent
simhyd_MJeq.sim <- function (DATA, rainfallInterceptionStoreCapacity = 1.5, infiltrationCoefficient = 200, 
                             infiltrationShape = 3, soilMoistureStoreCapacity = 320, interflowCoefficient = 0.1, 
                             rechargeCoefficient = 0.2, baseflowCoefficient = 0.3, perviousFraction = 0.9, 
                             imperviousThreshold = 1, groundwater_0 = 5, soilMoistureStore_0 = soilMoistureStoreCapacity * 
                               0.33, CONST_FOR_SOIL_ET = 10.0, return_state = FALSE, pure.R.code = FALSE) 
{
  inAttr <- attributes(DATA[, 1])
  DATA <- as.ts(DATA)
  stopifnot(c("P", "E") %in% colnames(DATA))
  stopifnot(rainfallInterceptionStoreCapacity >= 0)
  stopifnot(infiltrationCoefficient >= 0)
  stopifnot(infiltrationShape >= 0)
  stopifnot(soilMoistureStoreCapacity >= 0)
  stopifnot(interflowCoefficient >= 0)
  stopifnot(rechargeCoefficient >= 0)
  stopifnot(baseflowCoefficient >= 0)
  stopifnot(perviousFraction >= 0)
  stopifnot(imperviousThreshold >= 0)
  P <- DATA[, "P"]
  E <- DATA[, "E"]
  bad <- is.na(P) | is.na(E)
  P[bad] <- 0
  E[bad] <- 0
  X <- P
  COMPILED <- (hydromad.getOption("pure.R.code") == FALSE)
  if (COMPILED) {
    ans <- simhydMJEQ_sim(P,E,rainfallInterceptionStoreCapacity,
                          infiltrationCoefficient,
                          infiltrationShape,
                          soilMoistureStoreCapacity,
                          interflowCoefficient,
                          rechargeCoefficient,
                          baseflowCoefficient,
                          perviousFraction,
                          imperviousThreshold,
                          groundwater_0,
                          soilMoistureStore_0,
                          CONST_FOR_SOIL_ET)
      X <- ans$X
    if (return_state==T) {
      aET <- ans$aET
      imperviousRunoff = ans$imperviousRunoff
      interceptionET = ans$interceptionET
      throughfall = ans$throughfall
      infiltrationXSRunoff = ans$infiltrationXSRunoff
      infiltration = ans$infiltration
      interflowRunoff = ans$interflowRunoff
      infiltrationAfterInterflow = ans$infiltrationAfterInterflow
      soilMoistureFraction = ans$soilMoistureFraction
      soilMoistureStore = ans$soilMoistureStore
      soilInput = ans$soilInput
      recharge = ans$recharge
      groundwater = ans$groundwater
      soilET = ans$soilET
      baseflow = ans$baseflow
    }
   } else {
	 interceptionET <- infiltration <- throughfall <- infiltrationXSRunoff <- imperviousRunoff <- infiltrationAfterInterflow <- NA
      totalET <- soilET <- imperviousET <- interFlowRunoff <- recharge <- soilInput <- baseflow <- interception <- NA
      soilMoistureStore <- groundwater <- rep(0,length(P))
    groundwater[1] <- groundwater_0
    soilMoistureStore[1] <- soilMoistureStore_0
    for (t in seq(2, length(P))) {
      perviousIncident <- P[t]
      imperviousIncident <- P[t]
	  # equation 1
      imperviousET[t] <- min(imperviousThreshold, imperviousIncident)
      imperviousRunoff[t] <- imperviousIncident - imperviousET
	  # equation 2
      interceptionET[t] <- min(perviousIncident, min(E[t], 
                                                  rainfallInterceptionStoreCapacity))
      throughfall[t] <- perviousIncident - interceptionET
      soilMoistureFraction <- soilMoistureStore[t-1]/soilMoistureStoreCapacity
		# equation 3
      infiltrationCapacity <- infiltrationCoefficient * 
        exp(-infiltrationShape * soilMoistureFraction)
		# equation 4
      infiltration[t] <- min(throughfall[t], infiltrationCapacity) 
      infiltrationXSRunoff[t] <- throughfall[t] - infiltration[t]
		# equation 5
      interflowRunoff[t] <- interflowCoefficient * soilMoistureFraction * 
        infiltration[t]
		# equation 6
		infiltrationAfterInterflow[t] <- infiltration[t] - interflowRunoff[t]
		# equation 7
		recharge[t] <- rechargeCoefficient * soilMoistureFraction * 
        infiltrationAfterInterflow[t]
		# equation 8
		soilInput[t] <- infiltrationAfterInterflow[t] - recharge[t]
      soilMoistureStore[t] <- soilMoistureStore[t-1] + soilInput[t]
      soilMoistureFraction <- soilMoistureStore[t]/soilMoistureStoreCapacity
      groundwater[t] <- groundwater[t-1] + recharge[t]
      if (soilMoistureFraction > 1) {
        groundwater[t] <- groundwater[t] + soilMoistureStore[t] - 
          soilMoistureStoreCapacity
        soilMoistureStore[t] <- soilMoistureStoreCapacity
        soilMoistureFraction <- 1
      }
      baseflowRunoff[t] <- baseflowCoefficient * groundwater[t]
      groundwater[t] <- groundwater[t] - baseflowRunoff[t]
      soilET[t] <- min(soilMoistureStore[t], min(E[t] - interceptionET[t], 
               soilMoistureFraction * CONST_FOR_SOIL_ET))
      soilMoistureStore[t] <- soilMoistureStore[t] - soilET[t]
      eventRunoff <- (1 - perviousFraction) * imperviousRunoff[t] + 
        perviousFraction * (infiltrationXSRunoff[t] + interflowRunoff[t])
      totalET[t] = (1 - perviousFraction) * imperviousET[t] + perviousFraction * (interceptionET[t] + soilET[t]);
      totalRunoff <- eventRunoff + perviousFraction * baseflowRunoff[t]
      X[t] <- totalRunoff
    }
    aET <- totalET
   }
  # reset the missing values
  X[bad] <- NA
  if (return_state==T) {
    aET[bad] <- NA
    imperviousRunoff[bad] <- NA
    interceptionET[bad] <- NA
    infiltration[bad] <- NA
    throughfall[bad] <- NA
    infiltrationXSRunoff[bad] <- NA
    interflowRunoff[bad] <- NA
    infiltrationAfterInterflow[bad] <- NA
    soilET[bad] <- NA
    recharge[bad] <- NA
    soilInput[bad] <- NA
    baseflow[bad] <- NA
    soilMoistureStore[bad] <- NA
    groundwater[bad] <- NA
  }
# pass on attributes  
 attributes(X) <- inAttr
 ans <- X
 if (return_state==T) {
   attributes(aET) <- inAttr
   attributes(imperviousRunoff) <- inAttr
    attributes(interceptionET) <- inAttr
    attributes(infiltration) <- inAttr
    attributes(throughfall) <- inAttr
    attributes(infiltrationXSRunoff) <- inAttr
    attributes(interflowRunoff) <- inAttr
    attributes(infiltrationAfterInterflow) <- inAttr
    attributes(soilET) <- inAttr
    attributes(recharge) <- inAttr
    attributes(soilInput) <- inAttr
    attributes(baseflow) <- inAttr
    attributes(soilMoistureStore) <- inAttr
    attributes(groundwater) <- inAttr
    ans <- cbind(U=X,aET=aET, throughfall = throughfall,
				   imperviousRunoff = imperviousRunoff,
				   interceptionET = interceptionET,
				   infiltration = infiltration,
                   infiltrationXSRunoff = infiltrationXSRunoff,
                   interflowRunoff = interflowRunoff,
				   infiltrationAfterInterflow = infiltrationAfterInterflow,
				   soilInput = soilInput,
				   soilMoistureStore = soilMoistureStore,
				   recharge = recharge,
 				   groundwater=groundwater,
				   soilET = soilET,
          baseflow = baseflow)
 } 
 ans
}


# Routing based on Muskinghum
simhydrouting.sim <- function(U, DELAY=1, X_m=0.2, 
                              epsilon = hydromad.getOption("sim.epsilon"),
                              return_components = FALSE) {
  X <- rep(0,length(U))
  inAttr <- attributes(U)
  U <- as.ts(U)
  bad <- is.na(U)
  U[bad] <- 0
  if(2*DELAY*X_m<1 & 2*DELAY*(1-X_m)>1) {
    # Muskingum components
    C0 <- (-DELAY*X_m+0.5)/(DELAY*(1-X_m)+0.5)
    #print(C0)
    C1 <- (DELAY*X_m+0.5)/(DELAY*(1-X_m)+0.5)
    #print(C1)
    C2 <- (DELAY*(1-X_m)-0.5)/(DELAY*(1-X_m)+0.5)
    #print(C2)
  } else {
    C0 <- 0; C1 <- 1; C2 <- 0
   # print("model parameters adjusted")
  }
  
  if (C0 + C1 + C2 != 1) {    
    C0 <- 0; C1 <- 1; C2 <- 0
    # print("model parameters adjusted again")
  }
  #print(C0+C1+C2)
  #if (round(C0+C1+C2)!=1)  C0 <- 0; C1 <- 1; C2 <- 0
  
  X[1] <- U[1]
  for(t in 1:(length(U)-1)){ 
    X[t+1] <- C0*U[t+1]+C1*U[t]+C2*X[t]
    #print(X[t+1])
  }
  X[abs(X) < epsilon] <- 0
  X[bad] <- NA
  attributes(X) <- inAttr
  if (return_components) {
    return(X)
  }
  return(X)
}    

# define ranges of parameters
simhyd_C2009.ranges <- simhyd_C2002.ranges  <- function() 
  list(INSC = c(0,50),
       COEFF = c(0.0,400),
       SQ = c(0,10), 
       SMSC = c(1,500),
       SUB = c(0.0,1),
       CRAK = c(0.0,1),
       K = c(0.0,1),
        etmult = c(0.01,1))

simhydrouting.ranges <- function()
  list( DELAY = c(0.1,5),
       X_m = c(0.01,0.5))


simhyd_eWater.ranges <- function() 
  list(impTh = c(1,5),
       pFrac = c(0,1),
       INSC = c(0,50),
       COEFF = c(0.0,400),
       SQ = c(0,10), 
       SMSC = c(1,500),
       SUB = c(0.0,1),
       CRAK = c(0.0,1),
       K = c(0.0,1),
       etmult = c(0.01,1))


simhyd_MJeq.ranges <- function() 
  list(rainfallInterceptionStoreCapacity = c(0, 5),
           infiltrationCoefficient = c(0, 400),
           infiltrationShape = c(0, 10),
           soilMoistureStoreCapacity = c(1, 500),
           interflowCoefficient = c(0, 1),
           rechargeCoefficient = c(0, 1),
           baseflowCoefficient = c(0, 1),
           perviousFraction = c(0, 1),
           imperviousThreshold = c(0, 5))