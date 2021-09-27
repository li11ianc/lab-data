library(tidyverse)

bike <- read_csv("210830_bikecrash.csv")
# attempt to replicate these results

m1 <- glm(crashes ~ traffic_vol + pct_rural, 
          data = bike, 
          family = "poisson")
round(summary(m1)$coef[,1], 4)

crash <- bike$crashes
traf <- bike$traffic_vol

initial_guess <- 2

b_tplus1 <- initial_guess

i <- 1
for (i in 100) {
  exp(traf[i]*initial_guess) * traf[i] * t(traf[i])
}

(-exp(traf*initial_guess) %*% traf %*% t(traf))^-1


exp(traf[i]*initial_guess) * traf[i] * t(traf[i])


beta <- 2
X <- traf
y <- crash

calc.score <- function(beta, X, y){
  d1 <- rep(0, length(beta))
  for(i in 1:length(y)){
    d1 <- d1 + (y[i] - exp(X[i,] %*% beta)) %*% X[i,]
  }
  return(colSums(d1))
}

calc.hess <- function(beta, X, y){
  d1 <- rep(0, length(beta))
  for(i in 1:length(y)){
    d1 <- d1 + (exp(X[i,] %*% beta) %*% t(X[i,]) %*% X[i,])
  }
  return(-colSums(d1))
}

ex <- bind_cols(traf, bike$pct_rural) %>%
  janitor::clean_names()
calc.score(as.matrix(c(1,1)), as.matrix(ex), as.matrix(crash))
calc.hess



X <- as.matrix(ex)
X[1,]) %*% beta
beta <- as.matrix(c(2,3))
