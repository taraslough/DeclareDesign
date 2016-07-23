rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Quick Design")

test_that("quick_design with vary works", {

  simple_template <- function(N = 5000, 
                              n = 100, 
                              potential_outcomes_formula = Y ~ 5 + .5*Z*noise1 + noise2,
                              noise1 = "rnorm(n_)", noise2 = "rnorm(n_)",
                              potential_outcomes_condition_names = c(0, 1),
                              assignment_variable_name = "Z",
                              assignment_probability_each = c(.7, .3),
                              estimand_text = "mean(Y_Z_1 - Y_Z_0)",
                              estimator_function = difference_in_means,
                              estimator_formula = Y ~ Z
                              ){
    
    population <- declare_population(noise1 = noise1, noise2 = noise2, size = N)
    
    sampling <- declare_sampling(n = n)
    
    potential_outcomes <- 
      declare_potential_outcomes(formula = potential_outcomes_formula,
                                 condition_names = potential_outcomes_condition_names,
                                 assignment_variable_name = assignment_variable_name)
    
    assignment <- declare_assignment(potential_outcomes = potential_outcomes, 
                                     probability_each = assignment_probability_each,
                                     assignment_variable_name = assignment_variable_name)
    
    estimand <- declare_estimand(estimand_text = estimand_text, 
                                 potential_outcomes = potential_outcomes)
    
    estimator <- declare_estimator(estimates = estimator_function, 
                                   formula = estimator_formula, 
                                   estimand = estimand)
    
    design <- declare_design(population = population,
                             sampling = sampling, 
                             assignment = assignment, 
                             estimator = estimator, 
                             potential_outcomes = potential_outcomes,
                             label = "Simple Design")
    return(design)
  }
  
  # Check that single design works
  design <- quick_design(template = simple_template)
  expect_equal(class(design), "design")
  
  # Check that multiple designs with one vary work as expected
  expect_message(
    design_list1 <- quick_design(
      template = simple_template,
      assignment_probability_each = c(.7, .3),
      N = vary(50,75,100),
      potential_outcomes_formula = Y ~ 5 + .5*Z*noise2 + noise1,
      estimand_text = "mean(Y_Z_1 - Y_Z_0)"),
    " 3 ")
  
  expect_equal(length(design_list1$design_list), 3)
  expect_true(any(sapply(design_list1$design_list, FUN = class) == "design"))
  expect_true(colnames(design_list1$variable_labels) == "N")
  
  # Check that multiple designs with multiple vary work as expected
  expect_error(
    quick_design(
      template = simple_template,
      intersection = FALSE,
      assignment_probability_each = vary(c(.7, .3), c(.7, .4)),
      N = vary(50,75,100)
      ),
    "assignment_probability_each = 2, N = 3"
    )
  
  expect_message(
    design_list2 <- 
      quick_design(
        template = simple_template,
        intersection = FALSE,
        assignment_probability_each = vary(c(.7, .3), c(.7, .4)),
        N = vary(50,75)
      ),
    " 2 "
  )
  
  expect_equal(length(design_list2$design_list), 2)
  expect_true(any(sapply(design_list2$design_list, FUN = class) == "design"))
  expect_true(all(c("assignment_probability_each","N") %in% colnames(design_list2$variable_labels)))
  
  
  expect_message(design_list3 <- quick_design(
    template = simple_template,
    intersection = TRUE,
    assignment_probability_each = vary(c(.7, .3), c(.7, .4)),
    N = vary(50,75,100),
    potential_outcomes_formula = vary(Y ~ 5 + .5*Z*noise2 + noise1, Y ~ 2 + 2*Z - noise2 + noise1),
    noise1 = vary("rnorm(n_)", "rnorm(n_, sd = 2)", "rnorm(n_, mean = 2, sd = 10)")),
    " 36 ")
  
  expect_equal(length(design_list3$design_list), 2*3*2*3)
  expect_true(any(sapply(design_list3$design_list, FUN = class) == "design"))
  
})
