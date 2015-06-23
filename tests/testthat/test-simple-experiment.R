context("Basic experiment")

rm(list=ls())
library(testthat)
library(registration)

test_that("test whether a simple experiment can be pre-registered", {
  cov <- declare_covariates(
    individuals = list(
      income = declare_variable()),
    N_per_level = c(500))
  
  po     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(linear_mean = 0, linear_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .1*income
  )
  
  design        <- declare_design(N = 500, condition_names = po$condition_names)
  
  analysis_1      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                      design = design, method = "lm")
  
  power_1         <- get_power(sims = 100, analysis = analysis_1, design = design, potential_outcomes = po, covariates = cov)
  power_1
  
  mock          <- make_data(potential_outcomes = po, covariates = cov)
  mock$Z        <- assign_treatment(design)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", design = design,
                                    data = mock)
  M1             <- run_analysis(analysis = analysis_1, data = mock)  
  summary(M1)
  
  ## tmp 
  
  pre_register(design = design, covariates = cov, 
               potential_outcomes = po, analysis = analysis_1, 
               registration_title = "Simplest Possible Experiment", 
               registration_authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
               registration_abstract = "The effect of pixie dust on productivity.",
               random.seed = 42, temp_dir = TRUE, type = "rmarkdown",
               make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
               open_output = TRUE)
  
  
})
