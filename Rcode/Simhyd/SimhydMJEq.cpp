# include <Rcpp.h>
using namespace Rcpp;
// Based in Mun Ju Shin's/Felix Andrews code
// Provided by J Guillaume 2016


// [[Rcpp::export]]
DataFrame simhydMJEQ_sim(NumericVector P, NumericVector E, 
           double rainfallInterceptionStoreCapacity,
           double infiltrationCoefficient,
           double infiltrationShape,
           double soilMoistureStoreCapacity,
           double interflowCoefficient,
           double rechargeCoefficient,
           double baseflowCoefficient,
           double perviousFraction,
           double imperviousThreshold,
           double groundwater_0,
           double soilMoistureStore_0,
           double CONST_FOR_SOIL_ET)
{
  //size
  int n = P.size();
  
  NumericVector perviousIncident(n);
  NumericVector imperviousIncident(n);
  NumericVector imperviousET(n);
  NumericVector imperviousRunoff(n);
  NumericVector interceptionET(n);
  NumericVector throughfall(n);
  NumericVector soilMoistureFraction(n);
  NumericVector soilMoistureStore(n);
  NumericVector infiltrationCapacity(n);
  NumericVector infiltration(n);
  NumericVector infiltrationXSRunoff(n);
  NumericVector interflowRunoff(n);
  NumericVector infiltrationAfterInterflow(n);
  NumericVector recharge(n);
  NumericVector soilInput(n);
  NumericVector baseflowRunoff(n);
  NumericVector soilET(n);
  NumericVector totalStore(n);
  NumericVector totalET(n);
  NumericVector eventRunoff(n);
  NumericVector totalRunoff(n);
  NumericVector effectiveRainfall(n);
  NumericVector groundwater(n);
  NumericVector baseflow(n);
  NumericVector X(n);
  
  groundwater[0] = groundwater_0;
  soilMoistureStore[0] = soilMoistureStore_0;
  for (int t = 1; t < n; t++) {
    perviousIncident[t] = P[t];
    imperviousIncident[t] = P[t];
    imperviousET[t] = std::min(imperviousThreshold, imperviousIncident[t]);
    imperviousRunoff[t] = imperviousIncident[t] - imperviousET[t];
    interceptionET[t] = std::min(perviousIncident[t], std::min(E[t], rainfallInterceptionStoreCapacity));
    throughfall[t] = perviousIncident[t] - interceptionET[t];
    soilMoistureFraction[t] = soilMoistureStore[t-1] / soilMoistureStoreCapacity;
    infiltrationCapacity[t] = infiltrationCoefficient * exp(-infiltrationShape * soilMoistureFraction[t]);
    infiltration[t] = std::min(throughfall[t], infiltrationCapacity[t]);
    infiltrationXSRunoff[t] = throughfall[t] - infiltration[t];
    interflowRunoff[t] = interflowCoefficient * soilMoistureFraction[t] * infiltration[t];
    infiltrationAfterInterflow[t] = infiltration[t] - interflowRunoff[t];
    recharge[t] = rechargeCoefficient * soilMoistureFraction[t] * infiltrationAfterInterflow[t];
    soilInput[t] = infiltrationAfterInterflow[t] - recharge[t];
    soilMoistureStore[t] = soilMoistureStore[t-1] + soilInput[t];
    soilMoistureFraction[t] = soilMoistureStore[t]/ soilMoistureStoreCapacity;
    groundwater[t] = groundwater[t-1] + recharge[t];
    if( soilMoistureFraction[t] > 1 )
    {
      groundwater[t] = groundwater[t] + soilMoistureStore[t] - soilMoistureStoreCapacity;
      soilMoistureStore[t] = soilMoistureStoreCapacity;
      soilMoistureFraction[t] = 1;
    }
    baseflowRunoff[t] = baseflowCoefficient * groundwater[t];
    groundwater[t] = groundwater[t] - baseflowRunoff[t];
    soilET[t] = std::min(soilMoistureStore[t], std::min(E[t] - interceptionET[t], soilMoistureFraction[t] * CONST_FOR_SOIL_ET));
    soilMoistureStore[t] = soilMoistureStore[t] - soilET[t];
    totalStore[t] = soilMoistureStore[t] + groundwater[t];
    totalET[t] = (1 - perviousFraction) * imperviousET[t] + perviousFraction * (interceptionET[t] + soilET[t]);
    eventRunoff[t] = (1 - perviousFraction) * imperviousRunoff[t] + perviousFraction * (infiltrationXSRunoff[t] +	interflowRunoff[t]);
    totalRunoff[t] = eventRunoff[t] + perviousFraction * baseflowRunoff[t];
    effectiveRainfall[t] = P[t] - totalET[t];
    baseflow[t] = baseflowRunoff[t] * perviousFraction;
    X[t] = totalRunoff[t];
  }
// combine with ET in a matrix
    return DataFrame::create(Named("X")=X, Named("aET")=totalET,
                             Named("imperviousRunoff") = imperviousRunoff,
                             Named("interceptionET") = interceptionET,
                             Named("throughfall") = throughfall,
                             Named("infiltrationXsRunoff") = infiltrationXSRunoff,
                             Named("infiltration") = infiltration,
                             Named("interflowRunoff") = interflowRunoff,
                             Named("infiltrationAfterInterflow") = infiltrationAfterInterflow,
                             Named("soilMoistureFraction") = soilMoistureFraction,
                             Named("soilMoistureStore") = soilMoistureStore,
                             Named("soilInput") = soilInput,
                             Named("recharge") = recharge,
                             Named("groundwater")= groundwater,
                             Named("soilET") = soilET,
                             Named("baseflow") = baseflow);
}