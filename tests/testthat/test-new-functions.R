context("Check that basic code works")

rm(list=ls())
library(testthat)
library(preregister)

##test_that("test workflow", {
  
  ## dgp
  #  1 covs
  cov_dgp = function(means, sds, N_per_cluster = 10, f = rnorm) {
    # This is two level DGP; can be generalized
    L1 <- as.vector(sapply(1:length(means), function(j) rep(j, N_per_cluster)))
    L2 <- as.vector(sapply(1:length(means), function(j) f(N_per_cluster, means[j], sds[j])))
    data.frame(L1,L2)  
  }
  
  ## 2  add potential outcomes
  po_dgp = function(covs, betas = c(0,1), shockfn = function(x) rnorm(x,0,1), ate = 1){
    Y_0 <- as.matrix(covs)%*%as.matrix(betas) + shockfn(nrow(covs))
    Y_1 <- Y_0+ate
    data.frame(Y_0, Y_1)
  }
  
  # Sample implementation of everything
  
  covs          <- cov_dgp(c(1:3), c(1,1,0))
  ## this will be changed to declare_covariates (takes declare_dgp objects), then make_covariates
  ## this uses "level" as word rather than "cluster"
  
  podata        <- po_dgp(covs)
  
  design        <- declare_design(block_var = "L1")
  
  mock          <- cbind(podata, covs) ## temp
    ##make_data_frame(covariates = covs, potential_outcomes = podata)
  
  analysis      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", design = design, method = "lm")
  
  power         <- power(sims = 100, analysis = analysis, design = design, data = mock)
  
  mock$Z        <- assign_treatment(design)
  mock$Y        <- observed_y(outcome = "Y", treatment_assignment = "Z", data = mock)
  
  M             <- run_analysis(analysis = analysis, data = mock)  
  
  ##pre_register(covs, podata, design, mock, analyze, power)
  pre_register(design = design, data = mock, analysis = analysis,
               registration_title = "Lady Tasting Tea", 
               registration_authors = c("Ronald A. Fisher"), 
               registration_abstract = "Description of the lady tasting tea experiment",
               random.seed = 42, dir = getwd(), type = "rmarkdown",
               make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
               open_output = TRUE)
  
  
##})
