# -- Simulation function --
library(Rcpp)
sourceCpp("../Cpp/simulateC.cpp")

# -- TMB likelihood --
library(TMB)
compile("../Cpp/GARCH_likelihood.cpp")
dyn.load(dynlib("../Cpp/GARCH_likelihood"))

# -- Simulate --
true.par <- c(0.5, 0.2, .7)
x <- simulateC(n = 1e4, par = true.par, burnin = 1000)

# -- Estimate --
f <- MakeADFun(data = list(x = x),
               parameters = list(
                 omega = .45,
                 alpha = .1,
                 beta = .6
                 )
               )
fit <- nlminb(f$par, f$fn, f$gr, f$he, 
              lower = rep(1e-10, 3))
fit         
covar  <- solve(f$he(fit$par))
sd     <- sqrt(diag(covar))
result <- data.frame(par = c(0.5, 0.2, .7),
                     est = fit$par, 
                     sd = sd)
result

# -- Simulate using TMB --
x <- f$simulate(true.par)$x
f <- MakeADFun(data = list(x = x),
               parameters = list(
                 omega = .45,
                 alpha = .1,
                 beta = .6
               )
)
fit <- nlminb(f$par,f$fn,f$gr, f$he, lower = rep(1e-10, 3))
fit$par
