
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment with blocking and clustering")

test_that("test a simple experiment with blocking and clustering works with various functions", {
  sample <- declare_population(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(1000, 200))
  
  
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    formula = Y ~ .01 + 0*Z0 + .15*Z1 + .1*income + .15*Z1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  blocks <- declare_blocks(blocks = "development_level", recode = FALSE, clusters = clusters)
  
  assignment <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  analysis_2 <- declare_analysis(formula = Y ~ Z + income + development_level, treatment_variable = "Z", 
                                 estimator = "linear_regression", quantity_of_interest = average_treatment_effect)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", cluster_variable = "villages_id")
  
  ## estimated treatment effects
  
  pre_registration <- pre_register(assignment = assignment, sample = sample,
                                   potential_outcomes = potential_outcomes, analysis = analysis_1, 
                                   title = "Simplest Possible Experiment", 
                                   authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
                                   abstract = "The effect of pixie dust on productivity.",
                                   random_seed = 42, temp_dir = TRUE, open_output = FALSE)
  
  ## creates paper just from a pre_registration object
  ##draft_paper_from_pre_registration(pre_registration = pre_registration, data = mock)
  
  ##paper_draft <- draft_paper(assignment = assignment, sample = sample, clusters = clusters, blocks = blocks,
  ##                           potential_outcomes = potential_outcomes, analysis = analysis_1, 
  ##                           title = "Simplest Possible Experiment", 
  ##                           authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
  ##                           abstract = "The effect of pixie dust on productivity.",
  ##                           random_seed = 42, temp_dir = TRUE)
  
  ## nudge to set levels of sim (determined by assignment)
  
  power_1         <- diagnose_design(sims = 5, analysis = list(analysis_1), assignment = assignment, 
                                         sample = sample, potential_outcomes = potential_outcome)
  summary(power_1)
  
  power_2         <- diagnose_design(sims = 5, analysis = list(analysis_1, analysis_2), 
                                         assignment = assignment, sample = sample, potential_outcomes = potential_outcomes)
  summary(power_2)
  
  mock          <- make_data(potential_outcomes = potential_outcomes, sample = sample, assignment = assignment)
  
  head(mock)
  mock$Z        <- assign_treatment_indicator(assignment, data = mock)
  
  with(subset(mock, development_level==1), table(as.character(cluster_variable), block_variable))
  
  M1             <- get_estimands(analysis = analysis_1, data = mock)  
  summary(M1)
  
  mock$Y        <- reveal_outcome(outcome = "Y", treatment_assignment = "Z", data = mock)
  
  mock$prob_assign <- runif(nrow(mock))
  
  M1_est             <- get_estimates(analysis = analysis_1, data = mock)  
  summary(M1_est)
  
  ## below here doesn't work at the moment, working on it
  obs <- get_observed_assignment_probabilities(treatment_assignment = "Z", assignment = assignment, data = mock)
  
  balance       <- get_balance(covariates = c("income", "development_level"), 
                           treatment_assignment = "Z", assignment = assignment, data = mock)
  balance
  
})
