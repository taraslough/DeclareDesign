
rm(list=ls())
library(testthat)
library(experimentr)

context("Basic experiment with blocking")

test_that("test whether a simple blocked experiment can be pre-registered", {
  smp <- declare_sample(
    individuals = list(
      income = declare_variable(normal_mean = 3, normal_sd = 1),
      party = declare_variable(multinomial_probabilities = c(.5, .3, .2),
                               multinomial_categories = c("Dem", "Rep", "Ind"))),
    N = 2000)
  
  po     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income + runif(2000)
  )
  
  blocks <- declare_blocks(blocks = "income", block_name = "income_groups", block_count = 10)
  
  design_1        <- declare_design(potential_outcomes = po, blocks = blocks)
  design_2        <- declare_design(potential_outcomes = po, blocks = "party")
  
  mock_1          <- make_data(potential_outcomes = po, do_treatment_assignment = TRUE,
                             treatment_variable = "Z",
                             sample = smp, design = design_1)
  
  mock_2          <- make_data(potential_outcomes = po, do_treatment_assignment = TRUE,
                               treatment_variable = "Z",
                               sample = smp, design = design_2)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                 estimator = difference_in_means_blocked,
                                 block_variable = "income_groups")
  
  analysis_2 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                 estimator = difference_in_means_blocked,
                                 block_variable = "block_variable")
  
  power_test        <- diagnose(sims = 1000, 
                                       analysis = analysis_1, 
                                       design = design_1, 
                                       sample = smp, 
                                       potential_outcomes = po)
  
  
  fit_1 <- get_estimates(analysis = analysis_1, data = mock_1)
  
  fit_1 <- get_estimands(analysis = analysis_1, data = mock_1)
  
  summary(fit_1)
  
  #   pre_register(design = design, covariates = cov, 
  #                potential_outcomes = po, analysis = list(analysis_1, analysis_2), 
  #                registration_title = "Simplest Possible Experiment", 
  #                registration_authors = c("Graeme Blair", "Alexander Coppock"), 
  #                registration_abstract = "The effect of pixie dust on productivity.",
  #                random.seed = 42, temp_dir = TRUE, type = "rmarkdown",
  #                make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
  #                open_output = TRUE)
  #   
  
})
