
rm(list=ls())
library(testthat)
library(experimentr)

context("Basic experiment with blocking")

test_that("test whether a simple experiment with blocking can be pre-registered", {
  smp <- declare_sample_frame(
    individuals = list(
      income = declare_variable(normal_mean = 3, normal_sd = 1)),
    N_per_level = c(500))
  
  po     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(normal_mean = 0, normal_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income
  )
  
  blocks <- declare_blocks(blocks = "income")
  
  design        <- declare_design(potential_outcomes = po, blocks = blocks)
  
  analysis_1      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                      method = "lm")
  analysis_2      <- declare_analysis(formula = Y ~ Z + income, treatment_variable = "Z", 
                                      method = "lm")
  
  sims <- simulate_experiment(potential_outcomes = po, sample_frame = smp, blocks = blocks, 
                              design = design,analysis = list(analysis_1, analysis_2), sims = 10)
  summary(sims)
  
  # Run analysis on a single realization
  
  ## makes a data set with too few units
  mock          <- make_data(potential_outcomes = po, sample_frame = smp)
  
  ## does not work as a result (with blocks)
  mock          <- make_data(potential_outcomes = po, sample_frame = smp, blocks = blocks)
  mock$Z        <- assign_treatment(design, data = mock)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  probs_mat <- get_design_probs(design = design, data = mock)
  prob_obs <- observed_probs(treatment_assignment = "Z", design = design, data = mock)
  
  
  fit_1 <- get_estimates_model(analysis = analysis_1, data = mock)
  summary(fit_1)
  
  fit_2 <- get_estimates_model(analysis = analysis_2, data = mock)
  summary(fit_2)
  
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
