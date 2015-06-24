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
  
  cov <- declare_covariates(
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
  
  po     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .1*income + .1*female + .1*development_level,
    cluster_variable = "villages_id",
    ICC = .2
  )
  
  clusters <- declare_clusters(clusters = "villages")
  blocks <- declare_blocks(blocks = "districts", clusters = clusters)
     
  design        <- declare_design(potential_outcomes = po, blocks = blocks, clusters = clusters)
  
  analysis_1      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm", qoi_function = ?)
  analysis_2      <- declare_analysis(formula = Y ~ Z + income + female, treatment_variable = "Z", method = "lm", qoi_function = ?)
    
  ## where do you declare experiment-wide test_success function
  
  ## pre_register
  pre_register <- pre_register(design = design, covariates = cov, 
                               potential_outcomes = po, analysis = list(analysis_1, analysis_2), 
                               registration_title = "Lady Tasting Tea", 
                               registration_authors = c("Ronald A. Fisher"), 
                               registration_abstract = "Description of the lady tasting tea experiment",
                               random.seed = 42, dir = getwd(), type = "rmarkdown",
                               make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
                               open_output = TRUE)
  
  
  
  
  ## resample covariates and potential_outcomes
  sims         <- simulate_experiment(sims = 500, analysis = list(analysis_1, analysis_2), 
                                      design = design, covariates = cov, potential_outcomes = po, clusters = clusters, blocks = blocks)
  ## outputs experiment_simulations matrix
  
  summary <- summary(sims)
  ## outputs a table of summaries based on the matrix
  

  paper_draft(pre_registration = pre_register) ## outputs a paper draft Rmd / knitr / etc. file based on the pre-registered analyses etc.
    
  ## separately, you can test out how the analyses work, etc., using a realization of treatment
  ## now get a single realization of the data and treatment assignment
  mock          <- make_data(potential_outcomes = po, covariates = cov, clusters = clusters, blocks = blocks)
  
  data <- make_clusters(clusters, data)   ## sticks the cluster variable into data and returns the whole data frame
  data <- make_blocks(blocks, data)  ## sticks the blocks variable into data and returns the whole data frame
  
  mock$Z        <- assign_treatment(design)
  
  ## observed probabilities of treatment
  mock$Z_prob   <- observed_probs(treatment_assignment = "Z", design = design, data = mock)

  perm_matrix <- make_permutation_matrix(mock, treatment_assignment = "Z", design = design)
  
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", design = design,
                                    data = mock)

  
  
  M1             <- run_analysis(analysis = analysis_1, data = mock)  
  M2             <- run_analysis(analysis = analysis_2, data = mock)  
  
  
})




