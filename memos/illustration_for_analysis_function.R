rm(list=ls())


# The key analysis function takes declarations of functions of estimands and estimates
# that are each a function of data
# and returns a matrix (here, data frame) that includes all estimates, ses, and ps, for any data realization, 
# as well as estimands, which *may or may not* depend on data realizations

results = function(estimates  = default.estimates, 
                   estimands  = default.estimands, 
                   makedata = default.makedata,  runs=6) {
  
  out <- t(replicate(runs,  {data <- makedata()  
  c(as.vector(estimates(data)),  estimands(data))}
  ))
  
  n.ests <- length(estimands(makedata()))
  
  colnames(out) <-  c(as.vector(sapply(1:n.ests, function(j) {
    c(paste("bhat", j, sep=""),  paste("se", j, sep=""),  paste("p", j, sep=""))
  })), 
  paste("b", 1:n.ests, sep="")
  )        
  data.frame(out)   
}


# A default estimate would need to know names of treatments in order to function
# We might also include as defaults the blocking/ cluster structure implied by the design
# Or we could leave that to users
# Importantly this returns the p value of interest here; which will make the 
# power analysis easy. Also by returning the raw p, it would not be hard to return power for 
# a vector of alphas given a single set of simulations

default.estimates =  function(D) {
  coef(summary(lm(Y~Z1, data= D)))[2, -3]
}

# Need to specify default estimands, which requires information on structure of potential outcomes 
# For the factorial example I was playing with this turned out to be trickier than I expected
default.estimands =  function(D) { mean(D$Y1) - mean(D$Y0) }

# In practice though users can provide ANY estimates, including estimates from multiple distinct models
# And even multiple comparisons stats derived from those multiple models
# In this example, one model looks at the effects of changing two treatments simultaneously
# and another ignores the factorial nature of the design and includes a control
my.estimates = function(data)   {
  cbind(
    coef(summary(lm(Y~Z1, data = data[ ((data$Z1==1 & data$Z2==0) | (data$Z1==0 & data$Z2==0 )), ])))[2, -3],
    coef(summary(lm(Y~Z1 +X, data = data)))[2, -3]
   )
  }


# My estimands specify what I hope to get at
# There is no guarantee that the estimates estimate the estimands well
# Our package will answer that question in a natural way
my.estimands = function(data)  {c((mean(data$Y10-  data$Y01)), 
                                 mean(data$X==1) * ((mean(data$Y1[data$X==1])) -  (mean(data$Y0[data$X==1])) )+
                                 mean(data$X==0) * ((mean(data$Y1[data$X==0])) -  (mean(data$Y0[data$X==0])) )
                                 )
}
  
# makedata function 
default.makedata = function(n=10){
         Y00 <- runif(n)
         Y10 <- runif(n)
         Y01 <- runif(n)
         Y11 <- runif(n) +1
         Z1  <- rep(0:1, 5)
         Z2  <- sample(Z1)
         Y <- (1-Z1)*(1-Z2)*Y00 + (1-Z1)*Z2*Y01+Z1*(1-Z2)*Y10 + Z1*Z2*Y11  

         Y1 <- Y11*Z2 + Y10*(1-Z2)  # This is the PO on Y1, conditional on realized Z2
         Y0 <- Y01*Z2 + Y00*(1-Z2)  

         W1 <- Y11*Z1 + Y01*(1-Z1)  # This is the PO on Y2, conditional on realized Z1  
         W0 <- Y10*Z1 + Y00*(1-Z1)  
         X  <- 1*(runif(n) > .5)          
         data.frame(Y00, Y10, Y01, Y11, Z1,Z2, Y, Y1, Y0, W1, W0, X)
         }

example <- results(runs = 1000)

head(example)

# different power estimates can be gotten for the samerun for different alphas
mean(example$p1 < 0.05)
mean(example$p1 < 0.01)

# Lots of diagnostics possible
par(mfrow=c(2,2))
plot(example$bhat1, example$b1, xlab = "estimate", ylab = "estimand", main = "estimates and estimands")
# bias
hist(example$bhat1 - example$b1, main = "error distribution")
# sq error from population estimand
hist((example$bhat1 - example$b1)^2, 
     main = paste("sqd error (SATE); avg = ", round(mean((example$bhat1 - example$b1)^2),3)))
     
# sq error from population estimand
hist((example$bhat1 - mean(example$b1))^2, 
     main = paste("sqd error (PATE); avg = ", round(mean((example$bhat1 - mean(example$b1))^2),3)))


# Example wth multiple models 
example2 <- results(runs = 10, estimates = my.estimates, estimands = my.estimands)

head(example)
