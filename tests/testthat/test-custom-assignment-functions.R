
rm(list=ls())
library(testthat)
library(experimentr)

context("Assignment and probability functions")

test_that("test assignment and probability functions", {
  
  dat <- declare_population(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(1000, 200))$population
  
  
  pop <- declare_population(
    data = dat,
    N_per_level = c(1000, 200), super_population = TRUE)
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1", "Z2"),
    outcome_formula = Y ~ .01 + 0*Z0 + .15*Z1 + 0.2*Z2 + .1*income + .15*Z1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  blocks_with_clusters <- declare_blocks(blocks = "development_level", recode = FALSE, clusters = clusters)
  
  blocks_without_clusters <- declare_blocks(blocks = "development_level", recode = FALSE)
  
  assign_1 <- function()
    return(sample(c("Z0", "Z1"), 1000, replace = T))
  
  design <- declare_assignment(potential_outcomes = potential_outcomes, custom_assignment_function = assign_1)
 
  analysis <- declare_analysis(Y ~ Z, treatment_variable = "Z")
  
  sims <- diagnose(potential_outcomes = potential_outcomes, 
                   sample =  smp, 
                   design = design, 
                   analysis = analysis, 
                   sims = 100)
  
  mock <- make_data(potential_outcomes = potential_outcomes, sample = smp, design = design, 
                    assign_treatment = T, treatment_variable = "Z", observed_outcomes = TRUE)
 
})
