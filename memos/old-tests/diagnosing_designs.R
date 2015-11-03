
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Basic experiment with blocking")

test_that("test whether a simple blocked experiment can be pre-registered", {
  smp <- declare_sample(
    individuals = list(
      income = declare_variable(normal_mean = 3, normal_sd = 1)),
    N_per_level = c(10))
  
  po     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income
  )
  
  blocks <- declare_blocks(blocks = "income",block_count = 10)
  
  assignment        <- declare_assignment(potential_outcomes = po, blocks = blocks)
  
  mock          <- make_data(potential_outcomes = po, sample = smp, blocks = blocks)
  mock$Z        <- assign_treatment_indicator(assignment, data = mock)
  mock$Y        <- reveal_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  probs_mat <- get_assignment_probabilities(assignment = assignment, data = mock)
  prob_obs <- get_observed_assignment_probabilities(treatment_assignment = "Z", assignment = assignment, data = mock)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", estimator = linear_regression)
  
  fit_1 <- fit_model(analysis = analysis_1, data = mock)
  summary(fit_1)
  
  #   pre_register(assignment = assignment, covariates = cov, 
  #                potential_outcomes = po, analysis = list(analysis_1, analysis_2), 
  #                registration_title = "Simplest Possible Experiment", 
  #                registration_authors = c("Graeme Blair", "Alexander Coppock"), 
  #                registration_abstract = "The effect of pixie dust on productivity.",
  #                random.seed = 42, temp_dir = TRUE, type = "rmarkdown",
  #                make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
  #                open_output = TRUE)
  #   

  ## after this is a non-blocked assignment
  
  sample <- declare_sample(
    individuals = list(
      income = declare_variable(normal_mean = 3, normal_sd = 1)),
    N_per_level = c(5000))
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income + 5*income*Z1
  )
  
  assignment <- declare_assignment(potential_outcomes = potential_outcomes)
  
  analysis_lm <- declare_analysis(formula = Y ~ Z, estimator = linear_regression,
                                  quantity_of_interest = average_treatment_effect,
                                  estimand_formula = Y ~ Z)
  
  analysis <- declare_analysis(formula = Y ~ Z)
  
  power        <- diagnose(sims = 1000, analysis = list(analysis, analysis_lm), assignment = assignment, 
                                  sample = sample, 
                                  potential_outcomes = potential_outcomes)
  
    
})
