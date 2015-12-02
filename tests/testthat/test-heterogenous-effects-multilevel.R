rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Heterogenous effects")

test_that("test whether you can draw heterog effects in multilevel design", {
  
  original_args <- list(
    age_lambda = 30,
    city_educ_mu = 100,
    city_sigma = 10,
    city_educ_shape = 2,
    city_educ_rate = 2,
    party_vector = LETTERS[1:4]
  )
  
  population <- declare_population(
    indiv = list(
      income = "rnorm(n_)",
      age = "rpois(n_,age_lambda)",
      income_times_age = "income*age",
      party_ID = "sample(x = party_vector,size = n_,replace = TRUE)"
    ),
    city = list(
      city_educ_mean = "rnorm(n = n_, mean = city_educ_mu, sd = city_sigma)",
      city_educ_sd = "rgamma(n = n_, shape = city_educ_shape, rate = city_educ_rate)",
      city_educ_zscore = "scale(city_educ_mean)"
    ),
    region = list(),
    global_transformations = list(
      ml_educ = "rnorm(n_, mean = city_educ_mean, sd = city_educ_sd)"
    ),
    make_unique_ID = T,
    other_arguments = original_args,
    size = c(1000,100,10)
  ) 
  
  sampling <- declare_sampling(n = 100)
  
  multilevel_potential_outcomes_function <- function(formula, data){
    
    data$treat_effect_city <- rnorm(n = nrow(data), mean = data$city_educ_mean, data$city_educ_sd)
    
    return(eval(expr = formula[[3]], envir = data))
    
  }
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + Z*treat_effect_city + income,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z", 
                                                   potential_outcomes_function = multilevel_potential_outcomes_function)
  assignment <- declare_assignment(condition_names = c(0,1), probability_each = c(.7, .3))
  
  
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
  
  diagnosis <- diagnose_design(design = design)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes, condition_names = c(0, 1))
  head(smp_draw)  
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  # test draw_data
  
  smp_draw_reveal <- draw_data(design = design)
  
})

