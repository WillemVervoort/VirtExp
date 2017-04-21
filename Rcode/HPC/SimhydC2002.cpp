# include <Rcpp.h>
using namespace Rcpp;

// based on original Chiew et al. 2002
// Chiew, Murray & Peel, Chapter 11 in VP Singh
// Mathematical Models of Small Watershed Hydrology and Applications
// Water Resources Publication, 2002 - Technology & Engineering - 950 pages

// [[Rcpp::export]]
DataFrame simhydC2002_sim(NumericVector P, NumericVector E, 
                     double INSC, double COEFF,
                     double SQ,double SMSC,
                     double SUB, double CRAK,
                     double K) {
  //size
  int n = P.size();
  // storage output vector
	NumericVector U(n);
	// component flux vectors
	NumericVector INT(n), INR(n), RMO(n), IRUN(n);
	NumericVector tET(n), ET(n), SRUN(n), REC(n), SMF(n), POT(n), BAS(n);
	// Stores
	NumericVector SMS(n), GW(n);

	
	// initialise vectors
	SMS[0] = SMSC*0.5;
	GW[0] = 0.0;
	//Rcout << SMS[0];
	
	// run a loop
	for (int t = 1; t < n; ++t) {
 		// Interception follows Chiew et al. 2002
 		INT[t] = std::min(P[t],std::min(INSC,E[t]));
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
 		BAS[t] = K*GW[t];
 		GW[t] = GW[t] - BAS[t];
 		// Calculate total local runoff
 		U[t] = IRUN[t] + SRUN[t] + BAS[t];
// 		Rcout << U[t];
    // calculate total ET
    tET[t] =  ET[t] + INT[t];
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