
rm(list=ls())
library(testthat)
library(experimentr)

context("Assignment and probability functions")

test_that("test assignment and probability functions", {
  
  smp <- declare_sample(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(1000, 200))
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1", "Z2"),
    outcome_formula = Y ~ .01 + 0*Z0 + .15*Z1 + 0.2*Z2 + .1*income + .15*Z1*income
  )
  
  clusters <- declare_clusters(clusters = "villages_id")
  blocks_with_clusters <- declare_blocks(blocks = "development_level", recode = FALSE, clusters = clusters)
  
  blocks_without_clusters <- declare_blocks(blocks = "development_level", recode = FALSE)
  
  # Complete Random Assignment assignments
  assignment_1 <- declare_assignment(potential_outcomes = potential_outcomes)
  assignment_2 <- declare_assignment(potential_outcomes = potential_outcomes, m = 60, excluded_arms = "Z2")
  assignment_3 <- declare_assignment(potential_outcomes = potential_outcomes, m_each =c(60, 100, 840))
  assignment_4 <- declare_assignment(potential_outcomes = potential_outcomes, m_each =c(60, 940), excluded_arms = "Z2")
  assignment_5 <- declare_assignment(potential_outcomes = potential_outcomes, prob_each = c(.2, .5, .3))
  
  # Blocked assignments
  assignment_6 <- declare_assignment(potential_outcomes = potential_outcomes, blocks = blocks_without_clusters)
  assignment_7 <- declare_assignment(potential_outcomes = potential_outcomes, blocks = blocks_without_clusters, prob_each = c(.3, .6, .1))
  assignment_8 <- declare_assignment(potential_outcomes = potential_outcomes, blocks = blocks_without_clusters, excluded_arms = "Z2")
  
  block_prob <- rbind(c(.1, .2, .7),
                      c(.1, .7, .2),
                      c(.7, .2, .1),
                      c(.7, .1, .2),
                      c(.2, .1, .7))
  assignment_8.5 <- declare_assignment(potential_outcomes = potential_outcomes, 
                               blocks = blocks_without_clusters, 
                               block_prob = block_prob)
  
  # Clustered assignments 
  assignment_9 <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters)
  assignment_10 <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters, excluded_arms = "Z2")
  assignment_11 <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters, prob_each = c(.1, .3, .6))
  
  # Blocked and Clustered assignments
  assignment_12 <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks_with_clusters)
  assignment_13 <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks_with_clusters, excluded_arms = "Z2")
  assignment_14 <- declare_assignment(potential_outcomes = potential_outcomes, clusters = clusters, blocks = blocks_with_clusters, prob_each = c(.1, .3, .6))
  
  mock <- make_data(potential_outcomes = potential_outcomes, sample = smp, assignment = assignment_12)
  
  # Attempt to Assign
  mock$Z1 <- assign_treatment(assignment = assignment_1, data = mock) 
  mock$Z2 <- assign_treatment(assignment = assignment_2, data = mock) 
  mock$Z3 <- assign_treatment(assignment = assignment_3, data = mock) 
  mock$Z4 <- assign_treatment(assignment = assignment_4, data = mock) 
  mock$Z5 <- assign_treatment(assignment = assignment_5, data = mock) 
  
  mock$Z6 <- assign_treatment(assignment = assignment_6, data = mock) 
  mock$Z7 <- assign_treatment(assignment = assignment_7, data = mock) 
  mock$Z8 <- assign_treatment(assignment = assignment_8, data = mock) 
  mock$Z8_5 <- assign_treatment(assignment = assignment_8.5, data = mock) 
  
  with(mock, table(block_variable, Z8_5))
  
  mock$Z9 <- assign_treatment(assignment = assignment_9, data = mock) 
  mock$Z10 <- assign_treatment(assignment = assignment_10, data = mock) 
  mock$Z11 <- assign_treatment(assignment = assignment_11, data = mock) 
  
  mock$Z12 <- assign_treatment(assignment = assignment_12, data = mock) 
  mock$Z13 <- assign_treatment(assignment = assignment_13, data = mock) 
  mock$Z14 <- assign_treatment(assignment = assignment_14, data = mock) 
  
  # Obtain Treatment Probabilities
  
  prob_mat_1 <- get_assignment_probs(assignment = assignment_1, data = mock) 
  prob_mat_2 <- get_assignment_probs(assignment = assignment_2, data = mock) 
  prob_mat_3 <- get_assignment_probs(assignment = assignment_3, data = mock) 
  prob_mat_4 <- get_assignment_probs(assignment = assignment_4, data = mock) 
  prob_mat_5 <- get_assignment_probs(assignment = assignment_5, data = mock) 
  
  prob_mat_6 <- get_assignment_probs(assignment = assignment_6, data = mock) 
  prob_mat_7 <- get_assignment_probs(assignment = assignment_7, data = mock) 
  prob_mat_8 <- get_assignment_probs(assignment = assignment_8, data = mock) 
  
  prob_mat_8.5 <- get_assignment_probs(assignment = assignment_8.5, data = mock) 
  
  prob_mat_9 <- get_assignment_probs(assignment = assignment_9, data = mock) 
  prob_mat_10 <- get_assignment_probs(assignment = assignment_10, data = mock) 
  prob_mat_11 <- get_assignment_probs(assignment = assignment_11, data = mock) 
  
  prob_mat_12 <- get_assignment_probs(assignment = assignment_12, data = mock) 
  prob_mat_13 <- get_assignment_probs(assignment = assignment_13, data = mock) 
  prob_mat_14 <- get_assignment_probs(assignment = assignment_14, data = mock) 
  
  # reveal observed probs
  
  prob_obs_1 <- observed_probs(treatment_assignment = "Z1", assignment = assignment_1, data = mock) 
  prob_obs_2 <- observed_probs(treatment_assignment = "Z2", assignment = assignment_2, data = mock) 
  prob_obs_3 <- observed_probs(treatment_assignment = "Z3", assignment = assignment_3, data = mock) 
  prob_obs_4 <- observed_probs(treatment_assignment = "Z4", assignment = assignment_4, data = mock) 
  prob_obs_5 <- observed_probs(treatment_assignment = "Z5", assignment = assignment_5, data = mock) 
  
  prob_obs_6 <- observed_probs(treatment_assignment = "Z6", assignment = assignment_6, data = mock) 
  prob_obs_7 <- observed_probs(treatment_assignment = "Z7", assignment = assignment_7, data = mock) 
  prob_obs_8 <- observed_probs(treatment_assignment = "Z8", assignment = assignment_8, data = mock) 
  
  prob_obs_9 <- observed_probs(treatment_assignment = "Z9", assignment = assignment_9, data = mock) 
  prob_obs_10 <- observed_probs(treatment_assignment = "Z10", assignment = assignment_10, data = mock) 
  prob_obs_11 <- observed_probs(treatment_assignment = "Z11", assignment = assignment_11, data = mock) 
  
  prob_obs_12 <- observed_probs(treatment_assignment = "Z12", assignment = assignment_12, data = mock) 
  prob_obs_13 <- observed_probs(treatment_assignment = "Z13", assignment = assignment_13, data = mock) 
  prob_obs_14 <- observed_probs(treatment_assignment = "Z14", assignment = assignment_14, data = mock) 
})
