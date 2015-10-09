

# The estimand function looks for 
# 1. The user defined estimand that returns the estimand given the other arguments
# 2. The units for which estimand is defined
# 3. The treatment values for which the estimand is defined
# 4. The potential outcomes function, which may be a function of X's etc
# 5. Other optional unit level data 
estimand <-  function(user.estimand, po.function,  units = 1, zvals= c(0,1), X=NULL) user.estimand(zvals, po.function, units, X)


# Example 1: A factorial estimand: 
# Factorial estimand function returns interaction term, defaults to homogeneous treatment effects
user.estimand.f <- function(zvals, po.function, units = 1, X=NULL) {
  bs <- sapply(units, function(j) {
    Ys <- sapply(zvals, function(k)  po.function(j, k, X)) 
    return(Ys[4] - Ys[3] - Ys[2] + Ys[1])
  })
  return(mean(bs))
}

# Factorial potential outcomes function. Z takes values 1 = 00, 2 = 10, 3 = 01, 4 = 11
# In this example interaction term is -3
po.function.f <- function(i, z, X)  (z==2)  -2*(z==4)

# Apply
estimand(user.estimand = user.estimand.f,  po.function= po.function.f , units = 1,  zvals= 1:4)  


# Example 2: An estimand from continuous data with individual heterogeneity: 
# n
n <- 100

# Individual level source of heterogeneity
X <- runif(n)

# Continuous Potential Outcomes Function: Note het effects
po.function.c <- function(i, z, X) z*X[i] + z^2 + X[i]

# Estimand function returns OLS slope on true data, averages across units
user.estimand.c <- function(zvals, po.function, units = 1:n, X) {
  bs <- sapply(units, function(j) lm(po.function(j, zvals, X) ~ zvals)$coef[2])
  return(mean(bs))
}

# Apply
estimand(user.estimand = user.estimand.c, po.function= po.function.c,  units = 1:n,  zvals= 1:10,  X=X)  


# Example of a simple ATE for a binary treatment (even though potential outcomes more generally defined)
# Note this shows how the linear estimand is related to simplerATEs
estimand(user.estimand = user.estimand.c, po.function= po.function.c,  units = 1:n,  zvals= 0:1,  X=X)  
estimand(user.estimand = user.estimand.c, po.function= po.function.c,  units = 1:n,  zvals= 10:11,  X=X)  


