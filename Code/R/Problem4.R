# -- Problem 4 --
library(Rcpp)
library(TMB)
sourceCpp("../Cpp/simulateC.cpp")
compile("../Cpp/GARCH_likelihood.cpp")
dyn.load(dynlib("../Cpp/GARCH_likelihood"))

# -- Function that simulates and estimates a GARCH model --
# -- using the simulateC-function and TMB, respectively  --
run.montecarlo <- function(count = 1, 
                           true.par = c(.5,.2,.7), 
                           n = 1e4, 
                           burnin = 1000, 
                           print = FALSE){
  # -- Simulate --
  x <- simulateC(n = n, par = true.par, burnin = burnin)
  
  # -- Estimate --
  f <- MakeADFun(data = list(x = x),
                 parameters = list(
                   omega = abs(true.par[1]-.05),
                   alpha = abs(true.par[2]-.05),
                   beta = abs(true.par[3]-.05)
                 ),
                 silent = TRUE
  )
  fit <- nlminb(f$par,f$fn,f$gr, f$he, 
                lower = rep(1e-10, 3))
  if(print)
    cat(count,"\n")
  c(fit$par, 
    sqrt(diag(solve(f$he(fit$par)))))
}

# -- Alternative 1 -- 
#
# -- parSapply -- 
library(doParallel)
detectCores()
cores <- 40
cl    <- makeCluster(cores)

# -- Export to cluster: 
clusterEvalQ(cl, expr = {
  library(TMB)
  library(Rcpp)
  dyn.load(dynlib("../Cpp/GARCH_likelihood"))
  sourceCpp("../Cpp/simulateC.cpp")
})

# -- Run experiment --
true.par <- c(.5, .2, .7)

t1 <- Sys.time()
results <- parSapply(cl, X = 1:1e4, FUN = run.montecarlo)
t2 <- Sys.time()
t2 - t1
stopCluster(cl)

# -- Compare Monte Carlo mean to true parameter values: --
cbind(true.par, rowMeans(results)[1:3])
# -- Compare TMB hessian SD to Monte Carlo estimated SD: --
cbind(rowMeans(results)[4:6], apply(results[1:3,], 1, sd))

# -- Without parallel: --
# -- run experiment --
t1 <- Sys.time()
results <- sapply(X = 1:100, FUN = run.montecarlo)
t2 <- Sys.time()
t2 - t1

# -- Compare Monte Carlo mean to true parameter values: --
cbind(true.par, rowMeans(results)[1:3])
# -- Compare TMB hessian SD to Monte Carlo estimated SD: --
cbind(rowMeans(results)[4:6], apply(results[1:3,], 1, sd))


# -----------------------------------------------  
# -- Alternative 2 -- 
# -----------------------------------------------  
# -- FOREACH -- 
cl <- makeCluster(cores)

clusterEvalQ(cl, expr = {
  library(TMB)
  library(Rcpp)
  dyn.load(dynlib("../Cpp/GARCH_likelihood"))
  sourceCpp("../Cpp/simulateC.cpp")
})
registerDoParallel(cl)

# -- run experiment --
t1 <- Sys.time()
results <- 
  foreach(count = 1:100, .combine = cbind,
          .noexport = "simulateC") %dopar% {
    run.montecarlo()
}
t2 <- Sys.time()
t2-t1
stopCluster(cl)

# -- without parallel: replace %dopar% with %do%: --
t1 <- Sys.time()
results <- 
  foreach(count = 1:100, .combine = cbind, 
          .noexport = "simulateC") %do% {
    run.montecarlo()
}
t2 <- Sys.time()
t2-t1

# -- Compare Monte Carlo mean to true parameter values: --
cbind(true.par, rowMeans(results)[1:3])
# -- Compare TMB hessian SD to Monte Carlo estimated SD: --
cbind(rowMeans(results)[4:6], apply(results[1:3,], 1, sd))


