# include <Rcpp.h>
using namespace Rcpp;

// based on eWater version with impervious area
// There is a problem with the description of the model on the website
// and in the help manual.
// https://ewater.atlassian.net/wiki/display/SD41/SIMHYD+-+SRG
// The equations refer to an "impervious fraction" which is not defined
// or described in the text. The text is a straight copy of Chiew et al. 2002
// The model has also a "perviousIncident" presumably that fraction of the INR (below)
// that falls on the pervious fraction (pFrac below)
// There is also a imperviousThreshold (impTh below )
// for which estimates are given in the table
// on page 55 of the user manual. Fig 4.3 is somewhat clearer, but is different
// from the figure on the wiki or in the RRL software. 
// The code below is the best estimate of Fig 4.3

// [[Rcpp::export]]
DataFrame simhydeWater_sim(NumericVector P, NumericVector E,
                     double pFrac, double impTh,
                     double INSC, double COEFF,
                     double SQ,double SMSC,
                     double SUB, double CRAK,
                     double K) {
  //size
  int n = P.size();
  // storage output vector
	NumericVector U(n);
	// component flux vectors
	NumericVector ImpET(n), ImpQ(n);
	NumericVector IMAX(n), INT(n), INR(n), RMO(n), IRUN(n);
	NumericVector tET(n), ET(n), SRUN(n), REC(n), SMF(n), POT(n), BAS(n);
	// Stores
	NumericVector SMS(n), GW(n);

	
	// initialise vectors
	SMS[0] = SMSC*0.5;
	GW[0] = 0.0;
	//Rcout << SMS[0];
	
	// run a loop
	for (int t = 1; t < n; ++t) {
    // Impervious ET calculation
    double temp = std::min(E[t],(1 - pFrac)*impTh);
	  ImpET[t] = std::min(temp,(1- pFrac)*P[t]);
    // calculate impervious runoff as remainder
	  ImpQ[t] = 0.0;
	  if((1-pFrac)*P[t] - ImpET[t]>0) {
	    ImpQ[t] =  (1-pFrac)*P[t] - ImpET[t];
	  }
 		// Interception 
 		INT[t] = std::min(std::min(INSC,E[t]),pFrac*P[t]);
 		// interception runoff (INR), EXC in Chiew et al.2002
 		INR[t] = pFrac*P[t] - INT[t];
 		// Infiltration INF in Chiew et al. 2002
 		RMO[t] = std::min(COEFF*std::exp(-SQ*SMS[t-1]/SMSC),INR[t]);
 		// Direct runoff called SRUN in Chiew et al. 2002
 		IRUN[t] = INR[t] - RMO[t];
 		// Saturation Excess runoff (SRUN), INT, Interflow in Chiew et al. 2002
 		// calculated first according to Chiew et al. 2002
 		SRUN[t] = SUB*SMS[t-1]/SMSC*RMO[t];
 		// Recharge is estimated next according to Chiew et al. 2002
 		REC[t] = CRAK*SMS[t-1]/SMSC*(RMO[t]-SRUN[t]);
 		// Infiltration into soil store
 		SMF[t] = RMO[t] - SRUN[t] - REC[t];
 		// calulate SMS overflow (see figure 2 Chiew et al. 2009)
 		// calculate soil moisture balance
 		SMS[t] = SMS[t-1] + SMF[t];
 		if (SMS[t] > SMSC) {
 		  REC[t] = REC[t] + SMS[t] - SMSC; 
 		  SMS[t] = SMSC;
 		}
 		// calculate soil ET, Chiew et al. 2002 
 		ET[t] = std::min(10*SMS[t]/SMSC,E[t]);
 		SMS[t] = SMS[t] - ET[t];
 		// 		Rcout << SMS[t];
 		// calculate GW storage
 		GW[t] = GW[t-1] + REC[t];
 		//calculate baseflow
 		BAS[t] = K*GW[t-1];
 		GW[t] = GW[t] - BAS[t];
 		// Calculate total local runoff
 		U[t] = ImpQ[t] + IRUN[t] + SRUN[t] + BAS[t];
 		// Calculate total local Et
 		tET[t] = ImpET[t] + ET[t] + INT[t];
	}
 	// combine with ET in a matrix
 	  return DataFrame::create(Named("U")=U, Named("aET")=tET,
                             Named("INR") = INR,
                             Named("INT") = INT,
                             Named("IRUN") = IRUN,
                             Named("SRUN") = SRUN,
                             Named("RMO") = RMO,
                             Named("SMF") = SMF,
                             Named("SMS") = SMS,
                             Named("REC") = REC,
                             Named("GW")=GW,
                             Named("ET") = ET,
                             Named("BAS") = BAS);
}
 	