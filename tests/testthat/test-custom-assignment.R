rm(list=ls())
library(testthat)
library(DeclareDesign)

# add in a small test looking at how well assign_treatment() handles custom assignment functions
# 
# test also how well get_assignment_probabilties() fares with those same custom functions
# 
# add in tests for custom functions for:
#   
#   blocking
# clustering
# transforming
# blocking, clustering and transforming
# check multiple custom assignments work

<<<<<<< Updated upstream
context("Custom treatment assignment functions")

test_that("declare_assignment works with custom functions", {
  
  
  population <- declare_population(
    individuals = list(noise = "rnorm(n_)",
                       ideo_3 = "sample(c('Liberal', 'Moderate', 'Conservative'), size = n_, prob = c(.2, .3, .5), replace = T)"),
    villages = list(elevation = "rnorm(n_)",
                    high_elevation = "1*(elevation > 0)"), 
    size = c(1000, 100))
  
  sampling <- declare_sampling(n = 10, cluster_variable_name = "villages_ID")
  
  potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ 5 + .5*(Z==1) + .9*(Z==2) + .2*Z*elevation + noise,
    condition_names = c(0, 1, 2),
    assignment_variable_name = "Z")
  
  # Custom assignment function
  
  my_assignment <- function(data){
    N <- nrow(data)
    Z <- sample(x = c(1,2),size = N,replace = T)
    return(Z)
  }
  
  my_cluster_assignment <- function(data){
    N <- nrow(data)
    j <- data[,"villages_ID"]
    unique_j <- unique(j)
    j_z <- sample(unique_j,length(unique_j)%%2)
    Z <- j %in% j_z + 1
    return(Z)
  }
  
  # Custom blocking function 
  # Taken from test-blocked-clustered-experiment 
  my_block_function <- function(data) return(1 * (data$elevation > 0))

  my_cluster_function <- function(data) return(1 + (data$villages_ID))

  
  # Complete Random Assignment assignments
  assignment_1 <- declare_assignment(
    potential_outcomes = potential_outcomes, 
    custom_assignment_function = my_assignment)
  
  assignment_2 <- declare_assignment(
    potential_outcomes = potential_outcomes, 
    custom_assignment_function = my_cluster_assignment)
  
  assignment_3 <- declare_assignment(
    potential_outcomes = potential_outcomes,
    block_variable_name = "elevation", 
    custom_blocking_function = my_block_function
    )
  
  assignment_4 <- declare_assignment(
    potential_outcomes = potential_outcomes, 
    cluster_variable_name = "villages_ID",
    custom_clustering_function = my_cluster_function)
  
  # Draw Data
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  
  # Attempt to Assign
  smp_draw$Z1 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_1) 
  smp_draw$Z2 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_2) 
  smp_draw$Z3 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_3) 
  smp_draw$Z4 <- assign_treatment_indicator(data = smp_draw, assignment = assignment_4) 
  
  with(smp_draw, table(Z1))
  with(smp_draw, table(villages_ID, Z2))
  with(smp_draw, table(elevation, Z3))
  with(smp_draw, table(villages_ID, Z4))
  
  # Obtain Treatment Probabilities
  
  prob_mat_1 <- get_assignment_probabilities(data = smp_draw, assignment = assignment_1) 
  prob_mat_2 <- get_assignment_probabilities(data = smp_draw, assignment = assignment_2) 
  prob_mat_3 <- get_assignment_probabilities(data = smp_draw, assignment = assignment_3) 
  prob_mat_4 <- get_assignment_probabilities(data = smp_draw, assignment = assignment_4) 
  
  # reveal observed probs
  
  prob_obs_1 <- get_observed_assignment_probabilities(
    data = smp_draw, 
    assignment_variable_name = "Z1", 
    assignment = assignment_1) 
  prob_obs_2 <- get_observed_assignment_probabilities(
    data = smp_draw, 
    assignment_variable_name = "Z2", 
    assignment = assignment_2) 
  prob_obs_3 <- get_observed_assignment_probabilities(
    data = smp_draw, 
    assignment_variable_name = "Z3", 
    assignment = assignment_3) 
  prob_obs_4 <- get_observed_assignment_probabilities(
    data = smp_draw, 
    assignment_variable_name = "Z4", 
    assignment = assignment_4) 
=======
test_that("Custom Assignment Works", {
>>>>>>> Stashed changes
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = 1:4,
                                                   assignment_variable_name = "Z")
  
  custom_fun <- function(n_) sample(1:4, n_, replace=TRUE)
  
  assignment <- declare_assignment(custom_assignment_function = custom_fun)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           potential_outcomes = potential_outcomes)  
  
  smp_draw <- draw_data(design)
  head(smp_draw)
})

test_that("Custom Assignment again!", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0,1),
                                                   assignment_variable_name = "Z")
  
  custom_fun <- function(m, n_) ifelse(1:n_ %in% sample(1:n_, m-2), 1, 0)
  
  assignment <- declare_assignment(custom_assignment_function = custom_fun, m = 23)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           potential_outcomes = potential_outcomes)  
  
  smp_draw <- draw_data(design)
  head(smp_draw)
  table(smp_draw$Z)
})


test_that("Custom Assignment once again!", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0,1),
                                                   assignment_variable_name = "Z")
  
  custom_fun <- function(m, n_, distortion_factor) ifelse(1:n_ %in% sample(1:n_, m-distortion_factor), 1, 0)
  
  assignment <- declare_assignment(custom_assignment_function = custom_fun, m = 23, distortion_factor = 5)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           potential_outcomes = potential_outcomes)  
  
  smp_draw <- draw_data(design)
  head(smp_draw)
  table(smp_draw$Z)
})
