context("Basic experiment")

rm(list=ls())
library(testthat)
library(registration)

test_that("test assignment and probability functions", {
  
  smp <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(1000,200),
    lower_units_per_level = list(
      individuals = rep(1,1000), 
      villages = rep(5,200)
    ))
  
  potential_outcomes     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(linear_mean = 0, linear_sd = 1),
    condition_names = c("Z0","Z1", "Z2"),
    outcome_formula = Y ~ .01 + 0*Z0 + .15*Z1 + 0.2*Z2 + .1*income + .15*Z1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  blocks <- declare_blocks(blocks = "development_level", recode = FALSE, clusters = clusters)
  
  mock <- make_data(potential_outcomes = potential_outcomes, sample_frame = smp, blocks = blocks, clusters = clusters)
  
  
  # Complete Random Assignment Designs
  design_1 <- declare_design(potential_outcomes = potential_outcomes)
  design_2 <- declare_design(potential_outcomes = potential_outcomes, m = 60, excluded_arms = "Z2")
  design_3 <- declare_design(potential_outcomes = potential_outcomes, m_each =c(60, 100, 840))
  design_4 <- declare_design(potential_outcomes = potential_outcomes, m_each =c(60, 940), excluded_arms = "Z2")
  design_5 <- declare_design(potential_outcomes = potential_outcomes, prob_each = c(.2, .5, .3))
  
  # Blocked Designs
  design_6 <- declare_design(potential_outcomes = potential_outcomes, blocks = blocks)
  design_7 <- declare_design(potential_outcomes = potential_outcomes, blocks = blocks, prob_each = c(.3, .6, .1))
  design_8 <- declare_design(potential_outcomes = potential_outcomes, blocks = blocks, excluded_arms = "Z2")
  
  # Clustered Designs 
  design_9 <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters)
  design_10 <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, excluded_arms = "Z2")
  design_11 <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, prob_each = c(.1, .3, .6))
  
  # Blocked and Clustered Designs
  design_12 <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks)
  design_13 <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks, excluded_arms = "Z2")
  design_14 <- declare_design(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks, prob_each = c(.1, .3, .6))
  
  # Attempt to Assign
  mock$Z1 <- assign_treatment(design = design_1, data = mock) 
  mock$Z2 <- assign_treatment(design = design_2, data = mock) 
  mock$Z3 <- assign_treatment(design = design_3, data = mock) 
  mock$Z4 <- assign_treatment(design = design_4, data = mock) 
  mock$Z5 <- assign_treatment(design = design_5, data = mock) 
  
  mock$Z6 <- assign_treatment(design = design_6, data = mock) 
  mock$Z7 <- assign_treatment(design = design_7, data = mock) 
  mock$Z8 <- assign_treatment(design = design_8, data = mock) 
  
  mock$Z9 <- assign_treatment(design = design_9, data = mock) 
  mock$Z10 <- assign_treatment(design = design_10, data = mock) 
  mock$Z11 <- assign_treatment(design = design_11, data = mock) 
  
  mock$Z12 <- assign_treatment(design = design_12, data = mock) 
  mock$Z13 <- assign_treatment(design = design_13, data = mock) 
  mock$Z14 <- assign_treatment(design = design_14, data = mock) 
  
  # Obtain Treatment Probabilities
  
  prob_mat_1 <- get_design_probs(design = design_1, data = mock) 
  prob_mat_2 <- get_design_probs(design = design_2, data = mock) 
  prob_mat_3 <- get_design_probs(design = design_3, data = mock) 
  prob_mat_4 <- get_design_probs(design = design_4, data = mock) 
  prob_mat_5 <- get_design_probs(design = design_5, data = mock) 
  
  prob_mat_6 <- get_design_probs(design = design_6, data = mock) 
  prob_mat_7 <- get_design_probs(design = design_7, data = mock) 
  prob_mat_8 <- get_design_probs(design = design_8, data = mock) 
  
  prob_mat_9 <- get_design_probs(design = design_9, data = mock) 
  prob_mat_10 <- get_design_probs(design = design_10, data = mock) 
  prob_mat_11 <- get_design_probs(design = design_11, data = mock) 
  
  prob_mat_12 <- get_design_probs(design = design_12, data = mock) 
  prob_mat_13 <- get_design_probs(design = design_13, data = mock) 
  prob_mat_14 <- get_design_probs(design = design_14, data = mock) 
  
  # reveal observed probs
  
  prob_obs_1 <- observed_probs(treatment_assignment = "Z1", design = design_1, data = mock) 
  prob_obs_2 <- observed_probs(treatment_assignment = "Z2", design = design_2, data = mock) 
  prob_obs_3 <- observed_probs(treatment_assignment = "Z3", design = design_3, data = mock) 
  prob_obs_4 <- observed_probs(treatment_assignment = "Z4", design = design_4, data = mock) 
  prob_obs_5 <- observed_probs(treatment_assignment = "Z5", design = design_5, data = mock) 
  
  prob_obs_6 <- observed_probs(treatment_assignment = "Z6", design = design_6, data = mock) 
  prob_obs_7 <- observed_probs(treatment_assignment = "Z7", design = design_7, data = mock) 
  prob_obs_8 <- observed_probs(treatment_assignment = "Z8", design = design_8, data = mock) 
  
  prob_obs_9 <- observed_probs(treatment_assignment = "Z9", design = design_9, data = mock) 
  prob_obs_10 <- observed_probs(treatment_assignment = "Z10", design = design_10, data = mock) 
  prob_obs_11 <- observed_probs(treatment_assignment = "Z11", design = design_11, data = mock) 
  
  prob_obs_12 <- observed_probs(treatment_assignment = "Z12", design = design_12, data = mock) 
  prob_obs_13 <- observed_probs(treatment_assignment = "Z13", design = design_13, data = mock) 
  prob_obs_14 <- observed_probs(treatment_assignment = "Z14", design = design_14, data = mock) 
})


# should throw errors

design_1 <- declare_design(potential_outcomes = potential_outcomes, m_each =c(60, 940))
Z_1 <- assign_treatment(design = design_1, data = mock)  

design_3 <- declare_design(potential_outcomes = potential_outcomes, blocks = blocks, block_prob = c(.3, .7))
Z_3 <- assign_treatment(design = design_3, data = mock) 
