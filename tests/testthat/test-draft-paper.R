
rm(list=ls())
library(testthat)
library(experimentr)

context("Pre-registration and draft paper")

test_that("test draft paper and pre_register functions", {
  sample_frame <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(1000,200))
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .15*Z1 + .1*income + .15*Z1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  blocks <- declare_blocks(blocks = "development_level", recode = FALSE, clusters = clusters)
  
  design <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm")
  analysis_2 <- declare_analysis(formula = Y ~ Z + income + development_level, treatment_variable = "Z", 
                                 method = "lm")
  
  ## pre register experiment with EGAP
  
  pre_registration <- pre_register(design = design, sample_frame = sample_frame, clusters = clusters, blocks = blocks,
                                   potential_outcomes = potential_outcomes, analysis = analysis_1, 
                                   title = "Simplest Possible Experiment", 
                                   authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
                                   abstract = "The effect of pixie dust on productivity.",
                                   random_seed = 42, temp_dir = TRUE, open_output = FALSE)
  
  ## run the experiment, and create real_data (simulated here to demonstrate draft_paper)
  real_data          <- make_data(potential_outcomes = potential_outcomes, sample_frame = sample_frame, 
                                  blocks = blocks, clusters = clusters)
  real_data$Z        <- assign_treatment(design, data = real_data)
  real_data$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = real_data)
  
  ## create paper draft just from a pre_registration object
  draft_paper_from_pre_registration(pre_registration = pre_registration, data = real_data, 
                                temp_dir = TRUE, open_output = FALSE)
  
  ## or create custom paper draft from objects
  paper_draft <- draft_paper(design = design, sample_frame = sample_frame, clusters = clusters, blocks = blocks,
                             potential_outcomes = potential_outcomes, analysis = analysis_1, data = real_data,
                             title = "Simplest Possible Experiment", 
                             authors = c("Graeme Blair", "Jasper Cooper", "Alexander Coppock", "Macartan Humphreys"), 
                             abstract = "The effect of pixie dust on productivity.",
                             random_seed = 42, temp_dir = TRUE, open_output = FALSE)
  
  

})
