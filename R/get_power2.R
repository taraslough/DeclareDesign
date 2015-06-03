

get_power <- function(data, design, test_stat, sims=1000){
  
}

# series of test statistics

test_coefficients <- function(formula, which_coefs, data){
  
}
  
  
  

# Examples
design_1 <- declare_design(N = 100, m=50)
design_1

# A DGP -- NOT VERY GENERAL YET
dgp = function(T, condition_names, condition_means, condition_noise){

  sapply(1:length(T), function(i) condition_means[condition_names==T[i]]+condition_noise[condition_names==T[i]]*rnorm(1))
         
  }

# Example
dgp(c(1, 0, 1, 0, 1), c(0,1), c(2, 10), c(0, 1))


get_power <- function(design, condition_means, condition_noise, test_type = diff.means, alpha = 0.05, sims=1000){
  ps <- replicate(sims,
    {T <- design$ra_fun()
    Y  <- dgp(T, design$condition_names, condition_means, condition_noise) 
    t.test(Y, T)$p.value}
    )
  mean(ps <= alpha)
  }

# Example
design <- declare_design(N = 100, m=50)
get_power(design, c(2, 3), c(5, 5), test_stat = diff.means, alpha = 0.05, sims=1000)
get_power(design, c(2, 3), c(10, 10), test_stat = diff.means, alpha = 0.05, sims=1000)


# Comment 
# need dgp to be an argument entering flexibly
# need multiple test_types

