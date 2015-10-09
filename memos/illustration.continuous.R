
# n
n <- 100

# Individual level source of heterogeneity
X <- runif(n)

# Theoretically interesting distribution of Z: perhaps can be written as a function rather than here by simulation
Z <- rnorm(1000000)

# Continuos Potential Outcomes Function: Note het effects
po.function <- function(i, z, X) z*X[i] + z^2 + X[i]

# estimand is average effect of a change in Z FOR A SINGLE UNIT
estimand.function <- function(Y, Z) lm(Y~Z)$coef[2]
  

# Take distribtution of treatment,  po.function, estimand, and other data (X)  and returns estimand
get.estimand <- function(Z, po.function, estimand.function, X) {
  qs <- quantile(Z, (1:9)/10) 
  bs <- sapply(1:n, function(j) estimand.function(po.function(j, qs, X), qs) )
  return(mean(bs))
  }

get.estimand(Z, po.function, estimand.function, X)
get.estimand(Z, po.function, estimand.function, X)
get.estimand(Z, po.function, estimand.function, runif(n))


