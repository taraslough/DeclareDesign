
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("sampling and probability functions")

test_that("test sampling functions", {
  
  population <- declare_population(individuals = list(noise = declare_variable(),
                                                      ideo_3 = declare_variable(multinomial_probabilities = c(.2, .3, .5), 
                                                                                multinomial_categories = c("Liberal", "Moderate", "Conservative"))),
                                   villages = list(elevation = declare_variable(),
                                                   high_elevation = declare_variable(transformation = "1*(elevation > 0)")), 
                                   N_per_level = c(1000, 100))
  
  # Complete Random sampling
  sampling_0 <- declare_sampling()
  sampling_1 <- declare_sampling(prob = .5)
  sampling_2 <- declare_sampling(n = 100)
  
  # Blocked samplings
  sampling_3 <- declare_sampling(strata_variable_name = "ideo_3")
  sampling_4 <- declare_sampling(strata_variable_name = "ideo_3", strata_n = c(35, 40, 45))
  sampling_5 <- declare_sampling(strata_variable_name = "ideo_3", strata_prob = c(.1, .2, .3))
  
  # Clustered samplings 
  sampling_6 <- declare_sampling(cluster_variable_name = "villages_ID")
  sampling_7 <- declare_sampling(cluster_variable_name = "villages_ID", prob = .2)
  sampling_8 <- declare_sampling(cluster_variable_name = "villages_ID", n = 30)
  
  # Blocked and Clustered samplings
  sampling_9 <- declare_sampling(cluster_variable_name = "villages_ID", 
                                 strata_variable_name = "high_elevation")
  sampling_10 <- declare_sampling(cluster_variable_name = "villages_ID", 
                                  strata_variable_name = "high_elevation", 
                                  strata_prob = c(.1, .9))
  
  # Draw Data
  pop_draw <- draw_population(population = population)
  
  # Attempt to Assign
  pop_draw$S0 <- draw_sample_indicator(data = pop_draw, sampling = sampling_0) 
  pop_draw$S1 <- draw_sample_indicator(data = pop_draw, sampling = sampling_1) 
  pop_draw$S2 <- draw_sample_indicator(data = pop_draw, sampling = sampling_2) 
  
  with(pop_draw, table(S0))
  with(pop_draw, table(S1))
  with(pop_draw, table(S2))
  
  pop_draw$S3 <- draw_sample_indicator(data = pop_draw, sampling = sampling_3) 
  pop_draw$S4 <- draw_sample_indicator(data = pop_draw, sampling = sampling_4)
  pop_draw$S5 <- draw_sample_indicator(data = pop_draw, sampling = sampling_5)
  
  with(pop_draw, table(ideo_3, S3))
  with(pop_draw, table(ideo_3, S4))
  with(pop_draw, table(ideo_3, S5))
  
  pop_draw$S6 <- draw_sample_indicator(data = pop_draw, sampling = sampling_6) 
  pop_draw$S7 <- draw_sample_indicator(data = pop_draw, sampling = sampling_7) 
  pop_draw$S8 <- draw_sample_indicator(data = pop_draw, sampling = sampling_8) 
  
  with(pop_draw, table(S6, villages_ID))
  with(pop_draw, table(S7, villages_ID))
  with(pop_draw, table(S8, villages_ID))
  
  pop_draw$S9 <- draw_sample_indicator(data = pop_draw, sampling = sampling_9) 
  pop_draw$S10 <- draw_sample_indicator(data = pop_draw, sampling = sampling_10) 
  
  with(pop_draw, table(S9, villages_ID))
  with(pop_draw, table(S9, high_elevation))
  
  with(pop_draw, table(S10, villages_ID))
  with(pop_draw, table(S10, high_elevation))
  
  # Obtain Sampling Probabilities
  
  prob_1 <- get_inclusion_probs(data = pop_draw, sampling = sampling_1) 
  prob_2 <- get_inclusion_probs(data = pop_draw, sampling = sampling_2) 
  prob_3 <- get_inclusion_probs(data = pop_draw, sampling = sampling_3) 
  prob_4 <- get_inclusion_probs(data = pop_draw, sampling = sampling_4) 
  prob_5 <- get_inclusion_probs(data = pop_draw, sampling = sampling_5) 
  
  prob_6 <- get_inclusion_probs(data = pop_draw, sampling = sampling_6) 
  prob_7 <- get_inclusion_probs(data = pop_draw, sampling = sampling_7) 
  prob_8 <- get_inclusion_probs(data = pop_draw, sampling = sampling_8) 
  
  prob_9 <- get_inclusion_probs(data = pop_draw, sampling = sampling_9) 
  prob_10 <- get_inclusion_probs(data = pop_draw, sampling = sampling_10) 
  
})
