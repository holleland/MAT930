#include <TMB.hpp>     

template<class Type>
Type objective_function<Type>::operator() ()
{
  // -- Input: --
  DATA_VECTOR(x);  
  PARAMETER(omega);
  PARAMETER(alpha);
  PARAMETER(beta);
  
  // -- Constants: --
  Type m  = 0.0;
  Type sd = 1.0;
  int n   = x.size();
  
  // -- Initiating --
  vector<Type> h(n);
  h(0)   = omega;
  Type f = 0.0;
  
  // -- Calculate negative log-likelihood --
  for (int t = 0; t < n - 1; t++) {
    h(t+1) = omega + alpha * x(t) * x(t) + beta * h(t); 
    f     -= dnorm(x(t+1), m, sqrt(h(t+1)), true);
  }
  
  // -- Report h --
  REPORT(h);
  
  // -- Illustrating simulation capabilities --
  // -- Does not include burnin! --
  SIMULATE{
    h(0) = omega;
    for (int i = 0; i < n-1; i++) {
      h(i+1) = omega + alpha * x(i)*x(i) + beta * h(i); 
      x(i+1) = sqrt(h(i+1))  * rnorm(m, sd);
    }
    // -- Report from simulation --
    REPORT(h);
    REPORT(x);
  }
  // -- Return neg log likelihood --
  return f;
}

