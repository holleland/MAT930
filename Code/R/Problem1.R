# Source: http://adv-r.had.co.nz/Rcpp.html
library(Rcpp)

sumR <- function(x){
  n <- length(x)
  total <- 0
  for(i in 1:n)
    total <- total + x[i]
  total
}

cppFunction('
  double sumC(NumericVector x) {
    int n = x.size(); 
    double total = 0;
    for(int i = 0; i < n; i++){
      total = total + x[i];
    }
    return total;
  }')

# Test: 
x <- runif(10)
sumC(x)
sumR(x)
sum(x)

library(microbenchmark)

# -- 1000 numbers to sum -- 
x <- runif(1e3)
sumC(x)
(m1 <- microbenchmark(times = 100, check = "equal",
  sum(x),
  sumC(x),
  sumR(x)
  )
)

summary(m1)[2:3,4] / summary(m1)[1,4]

# -- 1 million numbers to sum -- 
x <- runif(1e6)
(m2 <- microbenchmark(times = 100, check = "equal",
               sum(x),
               sumC(x),
               sumR(x)
  )
)
summary(m2)[2:3,4]/summary(m2)[1,4]
