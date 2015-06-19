
context("Check covariate generation")

rm(list=ls())

test_that("make covariates works", {
    
  design_6 <- declare_design(clust_var=rep(letters, times=1:26), m_each=c(7, 7, 12),
                             condition_names=c("control", "placebo", "treatment"))
  
  covariate_object_1 <- make_covariates(
    X1 = declare_variable(),
    X2 = declare_variable(),
    event = declare_variable(binary_probability = .5,
                        binary_categories = c("happened","did not")),
    income = function()rnorm(n = design_6$N,mean = 0,sd = 1),
    count = function()rpois(n = design_6$N,lambda = 30),
    party_id = declare_variable(
      multinomial_probabilities = c(.4,.4,.2),
      multinomial_categories = c("D","R","I")),
    design_object = design_6
  )
  
  head(covariate_object_1$make_X_matrix())
  
})

test_that("declare_variable works", {

  skip("skipping now because not working")
  
  declare_variable()
  
  declare_variable(linear_mean = 1000,
              linear_sd = 100)
  
  declare_variable(variable_name = "Event",
              binary_probability = .8,
              binary_categories = c("Did not happen","Happened")
  )
  
  declare_variable(variable_name = "Party_ID",
              multinomial_probabilities = c(.4,.4,.2),
              multinomial_categories = c("D","R","I")
  )
  
})


test_that("make_potential_outcomes works", {
  
  skip("skipping now because not working")
  
  # Demo --------------------------------------------------------------------
  
  design_6 <- declare_design(clust_var=rep(letters, times=1:26), 
                             m_each=c(7, 7, 12),
                             condition_names=c("control", "placebo", "treatment")
  )
  
  covariate_object <- make_covariates(
    X1 = declare_variable(),
    X2 = declare_variable(),
    event = declare_variable(binary_probability = .5,
                        binary_categories = c("happened","did not")),
    income = function()rnorm(n = design_6$N,mean = 0,sd = 1),
    count = function()rpois(n = design_6$N,lambda = 30),
    design_object = design_6
  )
  
  potential_outcomes_linear <- 
    make_potential_outcomes(
      covariate_object = covariate_object,
      design_object = design_6,
      outcome_formula = y ~ 0*control + .05*placebo - .4*treatment + 
        .02*X1  + .02*X2  + 
        .02*(as.character(event)=="happened")  + 
        .02*income  + .02*count,
      ICC = .3
    )
  
  potential_outcomes_linear$make_outcomes()
  
  # Binary case
  
  potential_outcomes_binary <- 
    make_potential_outcomes(
      covariate_object = covariate_object,
      design_object = design_object,
      outcome_formula = y ~ 0*control + .05*placebo - .4*treatment + 
        .02*X1  + .02*X2  + 
        .02*(as.character(event)=="happened")  + 
        .02*income  + .02*count,
      outcome_DGP = declare_variable(binary_probability = .2), # Outcome declared binary here
      ICC = .3
    )
  
  potential_outcomes_binary$make_outcomes()
  
  
  # Pre-existing data:
  
  pre_existing_data <- covariate_object$make_X_matrix()
  
  potential_outcomes_data <- 
    make_potential_outcomes(
      covariate_object = pre_existing_data, # Supply a dataframe rather than a function
      design_object = design_object,
      outcome_formula = y ~ 0*control + .05*placebo - .4*treatment + 
        .02*X1  + .02*X2  + 
        .02*(as.character(event)=="happened")  + 
        .02*income  + .02*count,
      outcome_DGP = declare_variable(binary_probability = .2), # Outcome declared binary here
      ICC = .3
    )
  
  # Re-run this line a few times
  head(potential_outcomes_data$make_outcomes())
  # Note that because the cluster variable is regenerated, even though the covariate
  # matrix stays the same, the outcomes change slightly each time
  
  
})
