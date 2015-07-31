
rm(list=ls())
library(testthat)
library(registration)

context("Simple experiment with blocking and clustering")

test_that("test a simple experiment with blocking and clustering works with various functions", {
  sample_frame <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    lower_units_per_level = list(
      individuals = rep(1,1000), 
      villages = rep(5,200)
    ))
  
  potential_outcomes     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(normal_mean = 0, normal_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .15*Z1 + .1*income + .15*Z1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  blocks <- declare_blocks(blocks = "development_level", recode = FALSE, clusters = clusters)
  
  design <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm")
  analysis_2 <- declare_analysis(formula = Y ~ Z + income + development_level, treatment_variable = "Z", 
                                 method = "lm")
  
  ## estimated treatment effects
  
  pre_registration <- pre_register(design = design, sample_frame = sample_frame, clusters = clusters, blocks = blocks,
                                   potential_outcomes = potential_outcomes, analysis = analysis_1, 
                                   title = "Simplest Possible Experiment", 
                                   authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
                                   abstract = "The effect of pixie dust on productivity.",
                                   random_seed = 42, temp_dir = TRUE)
  
  ## creates paper just from a pre_registration object
  ##draft_paper_from_pre_register(pre_registration = pre_registration, data = mock)
  
  ##paper_draft <- draft_paper(design = design, sample_frame = sample_frame, clusters = clusters, blocks = blocks,
  ##                           potential_outcomes = potential_outcomes, analysis = analysis_1, 
  ##                           title = "Simplest Possible Experiment", 
  ##                           authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
  ##                           abstract = "The effect of pixie dust on productivity.",
  ##                           random_seed = 42, temp_dir = TRUE)
  
  ## nudge to set levels of sim (determined by design)
  
  power_1         <- simulate_experiment(sims = 5, analysis = list(analysis_1, analysis_2), design = design, 
                                         clusters = clusters, sample_frame = sample_frame, 
                                         potential_outcomes = potential_outcomes, blocks = blocks)
  summary(power_1)
  
  power_2         <- simulate_experiment(sims = 5, analysis = list(analysis_1, analysis_2), 
                                         design = design, clusters = clusters, blocks = blocks, 
                                         sample_frame = sample_frame, potential_outcomes = potential_outcomes)
  summary(power_2)
  
  mock          <- make_data(potential_outcomes = potential_outcomes, sample_frame = sample_frame, 
                             blocks = blocks, clusters = clusters)
  
  head(mock)
  mock$Z        <- assign_treatment(design, data = mock)
  
  with(subset(mock, development_level==1), table(as.character(cluster_variable), block_variable))
  
  M1             <- get_estimands(analysis = analysis_1, data = mock)  
  summary(M1)
  
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock)
  
  mock$prob_assign <- runif(nrow(mock))
  
  M1_est             <- get_estimates(analysis = analysis_2, data = mock)  
  summary(M1_est)
  
  ## below here doesn't work at the moment, working on it
  obs <- observed_probs(treatment_assignment = "Z", design = design, data = mock)
  
  balance       <- get_balance(covariates = c("income", "development_level"), 
                           outcome = "Y", treatment_assignment = "Z", design = design, data = mock)
  balance
  
})
