rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Noncompliance")

test_that("test whether noncompliance works", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 1000)
  sampling <- declare_sampling(n = 500)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*D,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "D")
  
  my_noncompliance_function <- function(
    data, 
    baseline_condition, 
    assignment_variable_name,
    prob_non_comply){
    
    N <- nrow(data)
    D <- data[,assignment_variable_name]
    non_comply <- rbinom(N,1,prob_non_comply)==1
    D[non_comply] <- baseline_condition
    
    return(D)
    
  }
  
  
  
  noncompliance <- declare_noncompliance(
    noncompliance_function = my_noncompliance_function,
    condition_names = c(0,1), 
    assignment_variable_name = "Z", 
    baseline_condition = 0,
    prob_non_comply = .1)
  
  assignment <- declare_assignment(condition_names = c(0,1))
  
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population,
                              potential_outcomes = list(potential_outcomes),
                              noncompliance = noncompliance)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes, noncompliance = noncompliance)
  
  head(pop_draw)
  head(smp_draw)
  with(smp_draw, table(Z, D))
  
})
