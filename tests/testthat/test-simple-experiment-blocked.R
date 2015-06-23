context("Basic experiment")

rm(list=ls())
library(testthat)
library(registration)

test_that("test whether a simple experiment with blocking can be pre-registered", {
  cov <- declare_covariates(
    individuals = list(
      income_cat = declare_variable(multinomial_probabilities = (1:5)/5)),
    N_per_level = c(5000))
  
  po     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(linear_mean = 0, linear_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .05*Z1 + .1*income_cat
  )
    
  mock          <- make_data(potential_outcomes = po, covariates = cov)
  
  block <- declare_blocks(blocks = "income")
  
  design        <- declare_design(block_var = mock$income_cat, condition_names = po$condition_names)
  
  analysis_1      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                      design = design, method = "lm")
  analysis_2      <- declare_analysis(formula = Y ~ Z + I(income_cat), treatment_variable = "Z", 
                                      design = design, method = "lm")
    
  ## resample covariates and potential_outcomes
  power_1         <- get_power(sims = 1, analysis = analysis_1, design = design, covariates = cov, potential_outcomes = po)
  
  power_1
  
  mock$Z        <- assign_treatment(design)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", design = design,
                                    data = mock)
  M1             <- run_analysis(analysis = analysis_1, data = mock)  
  summary(M1)
  
  ## tmp 
  
  pre_register(design = design, covariates = cov, 
               potential_outcomes = po, analysis = list(analysis_1, analysis_2), 
               registration_title = "Simplest Possible Experiment", 
               registration_authors = c("Graeme Blair", "Alexander Coppock"), 
               registration_abstract = "The effect of pixie dust on productivity.",
               random.seed = 42, temp_dir = TRUE, type = "rmarkdown",
               make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
               open_output = TRUE)
  
  
})
