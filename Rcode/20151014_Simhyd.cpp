# include <Rcpp.h>
using namespace Rcpp;

// based on Chiew et al. 2009

// [[Rcpp::export]]
DataFrame simhyd_sim(NumericVector P, NumericVector E, 
                     double INSC, double COEFF,
                     double SQ,double SMSC,
                     double SUB, double CRAK,
                     double K) {
  //size
  int n = P.size();
  // storage output vector
	NumericVector U(n);
	// component flux vectors
	NumericVector IMAX(n), INT(n), INR(n), RMO(n), IRUN(n);
	NumericVector ET(n), SRUN(n), REC(n), SMF(n), POT(n), BAS(n);
	// Stores
	NumericVector SMS(n), GW(n);

	
	// initialise vectors
	SMS[0] = SMSC*0.5;
	GW[0] = 0.0;
	//Rcout << SMS[0];
	
	// run a loop
	for (int t = 1; t < n; ++t) {
		// Interception storage, not in Chiew et al. 2002, unclear
 		IMAX[t] = std::min(INSC,E[t]);
 		// Interception 
 		INT[t] = std::min(IMAX[t],P[t]);
 		// interception runoff (INR), EXC in Chiew et al.2002
 		INR[t] = P[t] - INT[t];
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
 		// potential ET, based on Chiew et al. 2009 & Chiew 2006
 		// not in 2002, not in eWater Source
 		POT[t] = E[t] - INT[t];
 		// calculate soil ET, Chiew et al. 2002 and eWater use E[t] rather than POT[t]
 		ET[t] = std::min(10*SMS[t-1]/SMSC,POT[t]);
 		// calulate SMS overflow (see figure 2 Chiew et al. 2009)
 		// calculate soil moisture balance
 		SMS[t] = SMS[t-1] + SMF[t] - ET[t];
 		if (SMS[t] > SMSC) {
 		  SMS[t] = SMSC;
 		  REC[t] = REC[t] + SMS[t] - SMSC; 
 		}
// 		Rcout << SMS[t];
 		//calculate baseflow
 		BAS[t] = K*GW[t-1];
 		// calculate GW storage
 		GW[t] = GW[t-1] + REC[t] - BAS[t];
 		// Calculate total local runoff
 		U[t] = IRUN[t] + SRUN[t] + BAS[t];
// 		Rcout << U[t];
	}
 	// combine with ET in a matrix
 	return DataFrame::create(Named("U")=U, Named("aET")=E);
}