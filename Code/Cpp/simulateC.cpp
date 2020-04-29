#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector simulateC(int n, NumericVector par, int burnin) {
  NumericVector x(n+burnin);
  NumericVector sigma(n+burnin);
  NumericVector xr(n);
  sigma[0] = par[0];
  x[0] = sigma[0] * rnorm(1)[0];
  for(int t = 1; t < n+burnin; t++) {
    sigma[t] = sqrt(par[0] + par[1] * x[t-1] * x[t-1] + par[2] * sigma[t-1] * sigma[t-1]);
    x[t] = rnorm(1)[0] * sigma[t];
    if(t >= burnin){
      xr[t-burnin] = x[t];
    }
  }
  return xr;
}
  