rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = declare_variable(), N = 250)
  sampling <- declare_sampling(n = 100)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                   condition_names = c(0, 1),
                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(condition_names = c(0,1), probability_each = c(.7, .3))
  

# Diagnosis ---------------------------------------------------------------

  estimand <- declare_estimand(estimand_text = "mean(Y_1 - Y_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                    estimates_options = list(coefficient_name = "Z"),
                                    formula = Y ~ Z, estimand = estimand)
  get_regression_coefficient_vector <- function(model, formula = NULL, coefficient_name, 
                                                statistics = c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                                label = ""){
    
    coef_num <- which(names(coef(model)) == coefficient_name)
    df <- df.residual(model)
    est <- coef(model)[coef_num]
    se <- sqrt(diag(vcov(model)))[coef_num]
    p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
    conf_int <- suppressMessages(confint(model))[coef_num, ]
    
    output <- matrix(c(est, se, p, conf_int, df), 
                     dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                     paste0(summary(model)$terms[[2]], "~", paste(all.vars(summary(model)$terms[[3]]), collapse = "+"), "_", label)))[,1]
    
    return(output[which(names(output) %in% statistics)])
  }
  
  estimator_lm_vector <- declare_estimator(model = lm, estimates = get_regression_coefficient_vector, 
                                    estimates_options = list(coefficient_name = "Z"),
                                    formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_d_i_m, estimator_lm, estimator_lm_vector), 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
    
  diagnosis <- diagnose_design(design = design)
  
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
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  # test draw_data
  
  smp_draw_reveal <- draw_data(population = population, sampling = sampling, assignment = assignment, potential_outcomes = potential_outcomes)
  
})

