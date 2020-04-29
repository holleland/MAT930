library(Rcpp)
library(microbenchmark)
# --------
# -- R: --
# --------
simulateR <- function(n = 1000, par= c(.5,.3,.5), burnin = 100){
  sigma <- x <- numeric(n+burnin)
  sigma[1] <- sqrt(par[1])
  x[1] <- rnorm(1)*sigma[1]
  for(t in 2:(n+burnin)){
    sigma[t]<-sqrt(par %*% c(1,x[t-1]^2, sigma[t-1]^2))
    x[t]<-rnorm(1) * sigma[t]
  }
  return(x[-(1:burnin)])
}
# ----------
# -- Cpp: --
# ----------
sourceCpp("../Cpp/simulateC.cpp")

# -- Demonstrating that the two functions --
# -- produce the same outcome             --
set.seed(1)
plot(simulateR(n = 1000, par = c(.5, .3, .5), burnin = 100), 
     ylab ="Comparison of C++ and R code")
set.seed(1)
points(simulateC(n = 1000, par = c(.5, .3, .5), burnin = 100), 
       pch = 19)

# -- Performance comparsion: --
microbenchmark(times = 100,
  sc=simulateC(n = 1e4, par = c(.5,.3,.5), burnin = 1000),
  sr=simulateR(n = 1e4, par = c(.5,.3,.5), burnin = 1000)
              )


#-- Plot used in presentation: --
library(ggplot2)
df <- data.frame(x = simulateC(n = 1000, par = c(.5,.3,.5), burnin = 100),
           t = 1:1000)
ggplot(df, aes(x=t,y=x))+
  geom_line(col = rgb(172/255,203/255,249/255))+
  xlab("")+ylab("")+
  theme_minimal()+
  theme(rect = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.line = element_line(color = rgb(41/255,127/255,213/255)),
        axis.ticks = element_line(color = rgb(41/255,127/255,213/255)))
ggsave(filename = "../../garch.png",  bg = "transparent", width = 9, height = 4)


#-- Thanks to: --
citation("Rcpp")