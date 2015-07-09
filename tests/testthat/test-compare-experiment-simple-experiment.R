context("Basic experiment")

rm(list=ls())
library(testthat)
library(registration)

jasper_function <- function(a,b)a+b

test_that("test whether a simple experiment can be pre-registered", {
  
  jasper_function <- function(...){
    vars <- list(...)
    does stuff
    return(varaible_vector)
  }

  smp <- declare_sample_frame(N = 100,
                              individuals = list(
                                income = declare_variable(),
                                log_income = declare_variable(transformation = "recode(income, 1:5, 1:3)")
                                custom_var = declare_variable(transformation_variables = c("attitudes1", "attitudes2"), transformation_function = jasper_function),
                                attitudes1 = declare_variable(),
                                attitudes2 = declare_variable(),
                                attitudes_index = declare_variable(transformation = "attitudes1 + attitudes2") ## sum, mean, function
                                ))
  smp2 <- declare_sample_frame(N = 1500,
                               individuals=list(income = declare_variable()))
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income)
  
  design <- declare_design(potential_outcomes = po)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm")
  analysis_2 <- declare_analysis(formula = Y ~ Z + income, treatment_variable = "Z", method = "lm")
  
  sims <- simulate_experiment(potential_outcomes = po, sample_frame =  smp, 
                              design = design, analysis = analysis_1, sims = 5)
  
  comp_1 <- compare_experiments(design = design, analysis = analysis_1, po = po, 
                                sample_frame = list(smp, smp2), sims = 5)
  
  comp_2 <- compare_experiments(design = design, analysis = list(analysis_1, analysis_2),
                                po = po, sample_frame = smp, sims = 5)
  
  comp_3 <- compare_experiments(N = c(50, 100, 1000, 50000), design = design, analysis = analysis_1,
                                po = po, sample_frame = smp, sims = 100)
  
  summary(comp_2)
  
  # Run analysis on a single realization
  mock          <- make_data(potential_outcomes = po, sample_frame =  smp)
  mock$Z        <- assign_treatment(design, data = mock)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  estimates <- get_estimates(analysis = analysis_1,data = mock)
  estimates
  estimates <- get_estimates(analysis = analysis_2,data = mock)
  estimates
  
  fit_1 <- get_estimates_model(analysis = analysis_1, data = mock)
  fit_2 <- get_estimates_model(analysis = analysis_2, data = mock)
  summary(fit_1)
  summary(fit_2)
  
})
