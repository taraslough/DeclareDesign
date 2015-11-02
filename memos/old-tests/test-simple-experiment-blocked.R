
rm(list=ls())
library(testthat)
library(DeclareDesign)

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
    formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income + runif(2000)
  )
  
  blocks <- declare_blocks(blocks = "income", block_name = "income_groups", block_count = 10)
  
  assignment_1        <- declare_assignment(potential_outcomes = po, blocks = blocks)
  assignment_2        <- declare_assignment(potential_outcomes = po, blocks = "party")
  
  mock_1          <- make_data(potential_outcomes = po, assign_treatment_indicator =TRUE,
                             treatment_variable = "Z",reveal_outcomes = TRUE,
                             sample = smp, assignment = assignment_1)
  
  mock_2          <- make_data(potential_outcomes = po, assign_treatment_indicator =TRUE,
                               treatment_variable = "Z",reveal_outcomes = TRUE,
                               sample = smp, assignment = assignment_2)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                 estimator = difference_in_means_blocked,
                                 block_variable = "income_groups")
  
  analysis_2 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                 estimator = difference_in_means_blocked,
                                 block_variable = "block_variable")
  
  power_test_1        <- diagnose(sims = 20, 
                                analysis = analysis_1,
                                assignment = assignment_1,
                                sample = smp, 
                                potential_outcomes = po)
  
  power_test_2        <- diagnose(sims = 20, 
                                analysis = analysis_2,
                                assignment = assignment_2,
                                sample = smp, 
                                potential_outcomes = po)
  
  
  fit_1 <- get_estimates(analysis = list(analysis_1, analysis_2), data = mock_1)
  
  fit_1 <- get_estimands(analysis = analysis_1, data = mock_1)
  
  #   pre_register(assignment = assignment, covariates = cov, 
  #                potential_outcomes = po, analysis = list(analysis_1, analysis_2), 
  #                registration_title = "Simplest Possible Experiment", 
  #                registration_authors = c("Graeme Blair", "Alexander Coppock"), 
  #                registration_abstract = "The effect of pixie dust on productivity.",
  #                random.seed = 42, temp_dir = TRUE, type = "rmarkdown",
  #                make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
  #                open_output = TRUE)
  #   
  
})
