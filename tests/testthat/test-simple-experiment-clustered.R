context("Basic experiment with clustering")

rm(list=ls())
library(testthat)
library(registration)

test_that("test whether a simple experiment with clustering can be pre-registered", {
  smp <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    lower_units_per_level = list(
      individuals = rep(1,1000), 
      villages = rep(5,200)
    ))
  
  po     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(linear_mean = 0, linear_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .15*Z1 + .1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  
  design <- declare_design(potential_outcomes = po, clusters = clusters)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm")
  
  power_1         <- simulate_experiment(sims = 100, analysis = analysis_1, design = design, clusters = clusters, sample_frame = smp, potential_outcomes = po)
  power_1
  
  mock          <- make_data(potential_outcomes = po, sample_frame = smp, clusters = clusters)
  
  head(mock)
  mock$Z        <- assign_treatment(design, data = mock)
  
  with(mock, table(Z, cluster_variable))
  
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock)
  
  M1             <- get_estimates(analysis = analysis_1, data = mock)  
  summary(M1)
    
  ##pre_register(design = design, covariates = cov, 
  ##             potential_outcomes = po, analysis = analysis_1, 
  ##             registration_title = "Simplest Possible Experiment", 
  ##             registration_authors = c("Graeme Blair", "Alexander Coppock"), 
  ##             registration_abstract = "The effect of pixie dust on productivity.",
  ##             random.seed = 42, temp_dir = TRUE, type = "rmarkdown",
  ##             make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
  ##             open_output = TRUE)
  
})
