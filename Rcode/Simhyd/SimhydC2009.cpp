# include <Rcpp.h>
using namespace Rcpp;

// based on Chiew et al. 2009
// WRR VOL. 45, W10414, doi:10.1029/2008WR007338, 2009
// Chiew 2006 is the same, but with routing = NULL
// Hydrological Sciences-Journal-des Sciences Hydrologiques, 51(4) 2006

// [[Rcpp::export]]
DataFrame simhydC2009_sim(NumericVector P, NumericVector E, 
                     double INSC, double COEFF,
                     double SQ, double SMSC,
                     double SUB, double CRAK,
                     double K) {
  //size
  int n = P.size();
  // storage output vector
	NumericVector U(n);
	// component flux vectors
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
 		// calulate SMS overflow (see figure 2 Chiew et al. 2009)
 		// calculate soil moisture balance
 		SMS[t] = SMS[t-1] + SMF[t];
 		if (SMS[t] > SMSC) {
 		  REC[t] = REC[t] + SMS[t] - SMSC; 
 		  SMS[t] = SMSC;
 		}
 		// calculate soil ET, Chiew et al. 2002 and eWater use E[t] rather than POT[t]
 		ET[t] = std::min(10*SMS[t]/SMSC,POT[t]);
 		SMS[t] = SMS[t] - ET[t];
 		// 		Rcout << SMS[t];
 		// calculate GW storage
 		GW[t] = GW[t-1] + REC[t];
 		//calculate baseflow
 		BAS[t] = K*GW[t];
 		GW[t] = GW[t]  - BAS[t];
 		// Calculate total local runoff
 		U[t] = IRUN[t] + SRUN[t] + BAS[t];
 		// calculate total ET
 		tET[t] =  ET[t] + INT[t];
// 		Rcout << U[t];
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
                             Named("GW")= GW,
                             Named("ET") = ET,
                             Named("BAS") = BAS);
}