
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Basic experiment with clustering")

test_that("test whether a simple experiment with clustering can be pre-registered", {
  make_clustered_data <- function(J = 10, n = 100, treatment_effect = .25, ICC = .1){
    ## Inspired by Mathieu et al, 2012, Journal of Applied Psychology
    if (J %% 2 != 0 | n %% 2 !=0) {
      stop(paste("Number of clusters (J) and size of clusters (n) must be even."))
    }
    Y0_j         <- rnorm(J,0,sd = (1 + treatment_effect) ^ 2 * sqrt(ICC))
    fake_data    <- expand.grid(i = 1:n,j = 1:J)
    fake_data$Y0 <- rnorm(n * J,0,sd = (1 + treatment_effect) ^ 2 * sqrt(1 - ICC)) + Y0_j[fake_data$j]
    fake_data$Y1 <- with(fake_data,mean(Y0) + treatment_effect + (Y0 - mean(Y0)) * (2 / 3))
    fake_data$Z  <- ifelse(fake_data$j %in% sample(1:J,J / 2) == TRUE, 1, 0)
    fake_data$Y  <- with(fake_data, Z * Y1 + (1 - Z) * Y0)
    return(fake_data)
  }
  
  make_clustered_data_DeclareDesign <- function(N_per_level = NULL){
    if(is.null(N_per_level))
    if(N_per_level[1] %% N_per_level[2] > 0)
      stop("You can only use a assignment with equally sized clusters.")
    if(length(N_per_level)!=2)
      stop("You can only use this custom DGP function if there are exactly two levels.")
    return(make_clustered_data(J = N_per_level[2], n = N_per_level[1]/N_per_level[2]))
  }
  
  
  smp <- declare_sample(custom_data_function = make_clustered_data_DeclareDesign, N_per_level = c(1000, 10))
  
  po     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    formula = Y_star ~ Y0*Z0 + Y1*Z1 
  )
  
  clusters <- declare_clusters(clusters = "j")
  
  assignment <- declare_assignment(potential_outcomes = po, clusters = clusters, treatment_variable = "Z_star")
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  
  power_1         <- diagnose(sims = 100, analysis = analysis_1, assignment = assignment, clusters = clusters, sample = smp, potential_outcomes = po)
  power_1
  
  mock          <- make_data(potential_outcomes = po, sample = smp, assignment = assignment, assign_treatment_indicator = T, reveal_outcomes = T, treatment_variable = "Z_star")
  
})
