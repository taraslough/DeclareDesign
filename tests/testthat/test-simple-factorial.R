# A simple factorial design 
# Estimand is difference in difference estimator

# Second a design in which T1 and T2 are inependently assigned

rm(list=ls())
library(testthat)
library(DeclareDesign)

context("A simple factorial design ")

test_that("Factorial", {
  
  # Factorial
  population <- declare_population(noise = declare_variable(),  size = 500)
  
  
  # Estimand of interest with be the diff in diff = 1
  potential_outcomes <- declare_potential_outcomes(condition_names = c("Z00", "Z10", "Z01", "Z11"), 
                                                   formula = Y ~ (Z == "Z11") + 1 * noise)
  
  sampling <- declare_sampling(n = 100)
  
  my_estimand    <- function(data) { mean(data$Y_Z_Z11 - data$Y_Z_Z10  - data$Y_Z_Z01 + data$Y_Z_Z00) }
  
  estimand       <- declare_estimand(estimand_function = my_estimand, potential_outcomes = potential_outcomes)
  
  assignment     <- declare_assignment(potential_outcomes = potential_outcomes)
  
  
  my_estimates   <- function(data) { 
    data$T1 <- data$Z == "Z01" |data$Z == "Z11" 
    data$T2 <- data$Z == "Z10" |data$Z == "Z11" 
    M        <- lm(Y~T1*T2, data = data)
    stats    <- coef(summary(M))[4,]
    est      <- stats[1]
    se       <- stats[2]
    df       <- M$df.residual
    p        <- stats[4]
    ci_lower <- est - 1.96*se
    ci_upper <- est + 1.96*se
    t(matrix(c(est, se, p, ci_lower, ci_upper, df), 
           dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), NULL))) }
  
  estimator      <- declare_estimator(estimates = my_estimates, estimand = estimand)
  
  
  my_design <- declare_design(population = population, 
                              sampling = sampling, assignment = assignment, estimator = estimator, 
                              potential_outcomes = potential_outcomes, label = "simple_design")
  
  summary(my_design)
  
  
  diagnosis <- diagnose_design(design = my_design, population_draws = 5, 
                               sample_draws = 2)
  
  diagnosis
  
  
  data <- draw_data(my_design)
  
  my_estimates(data)
  
})
