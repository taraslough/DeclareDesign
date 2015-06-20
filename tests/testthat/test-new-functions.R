context("Check that basic code works")

rm(list=ls())
library(testthat)
library(registration)

test_that("test workflow", {
  
  # Takes arbitrary number of nested levels (will implement non-nested levels later)
  # You either simply list all of your covariates (for a 1-level data structure),
  # or put each successive level in as a list of variables contained at that level 
  # (i.e. the unit-level variables, the cluster-level variables, the block-level variables)
  # There is some trickiness about how to automate the numeric descriptions of the number of
  # levels within levels a bit better. 
  # Level ids generated automatically depending on how the list is named. 
  # It should be noted that the function does not produce dataframes that are sensitive to ordering.
  # Also user-programmed RNGs can be used in place of DGP_objects made with declare_variable()
  
  cov_object <- declare_covariates(
    individuals = list(
      income = declare_variable(),
      female = declare_variable(binary_probability = .5)),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    districts = list(
      district = function()sample(1:4)
    ),
    N_per_level = c(100,20,4),
    lower_units_per_level = list(
      individuals = rep(1,100), 
      villages = rep(5,20),
      blocks = rep(5,4)
    ))
  
  # You can declare an arbitrary number of potential outcomes using condition_names and outcome_formula
  
  po_object     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .1*income + .1*female + .1*development_level,
    cluster_variable = "villages_id",
    ICC = .2
  )
  
  # Make data is flexible: it can take just a covariate_object, just a PO_object 
  # (this substitutes standard normals for the covariates in the formula), or both 
  # the potential_outcomes and the covariate_object. It can also handle fixed covariates.
  
  mock          <- make_data(potential_outcomes = po_object, covariates = cov_object)
  
  design        <- declare_design(clust_var = mock$village,
                                  block_var = mock$district_id,
                                  condition_names = po_object$condition_names)
  
  analysis_1      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                      design = design, method = "lm")
  analysis_2      <- declare_analysis(formula = Y ~ Z + income + female, treatment_variable = "Z",
                                      design = design, method = "lm") ## "robustness check"
  
  power_1         <- get_power(sims = 100, analysis = analysis_1, design = design, data = mock)
  power_2         <- get_power(sims = 100, analysis = analysis_2, design = design, data = mock)
  
  mock$Z        <- assign_treatment(design)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", design = design,
                                    data = mock)
  
  M1             <- run_analysis(analysis = analysis_1, data = mock)  
  M2             <- run_analysis(analysis = analysis_2, data = mock)  
  
  pre_register(design = design, covariates = cov_object, 
               potential_outcomes = po_object, analysis = analysis_1, 
               registration_title = "Lady Tasting Tea", 
               registration_authors = c("Ronald A. Fisher"), 
               registration_abstract = "Description of the lady tasting tea experiment",
               random.seed = 42, dir = getwd(), type = "rmarkdown",
               make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
               open_output = TRUE)
  
  
})
