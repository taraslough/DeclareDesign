context("Check that basic code works")

rm(list=ls())
library(testthat)

test_that("test workflow", {
  
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
    data.frame(covs, Y_0, Y_1)
  }
  
  # randomization functions; use randomizr here
  ra.complete <- function(id=1:10, p){
    li      <- length(id)
    min     <- floor(p*li)
    target  <- min + (runif(1)< (p*li-min))
    selecti <- sample(c(rep(1, target), rep(0, li-target))) 
    selecti
  }
  
  ra.block <- function(block, p){
    out<-rep(NA, length(as.numeric(block)))
    for(b in unique(block)) {
      out[block==b] <-   ra.complete(block[block==b], p = p)
    }
    out
  }
  
  ## Design is where blocks, clusters are identified, etc
  declare_design <- function(propensity = .5, block = TRUE, block_var = 1:2, n) {
    if(!block) f   <-  function(p = propensity) ra.complete(1:n, propensity)
    if(block)  f   <-  function(p = propensity) ra.block(block_var, propensity)  
    return(f)
  }
    
  ##source("R Package scripts/declare_design.R")
  
  # Mock data, depends on podata and  design
  ##mock_data = function(podata, design){
  ##  Z <- design()
  ##  Y <- ifelse(Z==1, podata$Y1, podata$Y0)
  ##  data.frame(Y=Y, Z=Z, podata)
  ##}
  
  # Analysis; this is currently a little redundant
  # It should take block and clusters as arguments
  ##analysis_func <- function(data, f=lm, block.analyis= TRUE, block.analysis.name = "L1") {
  ##  if(!block.analyis)   function(data) f(Y~Z, data = data)
  ##  if(block.analyis)    function(data) f(paste("Y~Z+", block.analysis.name), data = data)
  ##}
  
  # power analysis -- depends on how far back one wants to go
  # This one takes the covariates as known but not the potential outcomes 
  
  ## data, analysis, test, design
  power = function(sims = 100, alpha = .05,                 # Power arguments
                   covs,   n = nrow(covs),                  # Existing data arguments   
                   betas = c(0,1), ate=1,                   # DGP arguments 
                   block = TRUE, block_var = "L1",          # Design arguments                   
                   analysis,                                # Analysis arguments
                   design,
                   block.analyis= TRUE, 
                   block.analysis.name = "L1"
  ) {
    design <- declare_design(block = block, block_var = covs[block_var][,1], n = n)
    ps <- sapply(1:sims, function(i){
      podata   <- po_dgp(covs, betas = betas, ate = ate)
      mock     <- make_data_frame(covariates = covs, potential_outcomes = podata)
      mock$Z        <- assign_treatment(design)
      mock$Y        <- observed_y(outcome = "Y", treatment.assignment = "Z", data = mock)      
      test_success   <- test_success(analysis = analysis, data = mock)
      
      return(test_success)
    }
    )
    mean(ps)
  }
  
  assign_treatment <- function(design) {
    return(design())
  }
  
  observed_y <- function(outcome, treatment.assignment, data){
    observed_y <- rep(NA, nrow(data))
    treat.vals <- na.omit(unique(data[,treatment.assignment])) ## temporarily doing na.omit
    ##if(any(is.na(treat.vals)))
    ##  stop("There are NAs in the treatment assignment.")
    for(v in treat.vals){
      treat.cond <- data[,treatment.assignment] == v & 
        is.na(data[,treatment.assignment]) == F ## is.na is temporary
      observed_y[treat.cond] <- data[treat.cond, paste(outcome, "_", v, sep = "")]
    }
    return(observed_y)
  }
  
  
  # Sample implementation of everything
  
  covs          <- cov_dgp(c(1:3), c(1,1,0))
  ## this will be changed to declare_covariates (takes declare_dgp objects), then make_covariates
  ## this uses "level" as word rather than "cluster"
  
  podata        <- po_dgp(covs)
  
  design        <- declare_design(block_var = covs$L1)
  
  mock          <- make_data_frame(covariates = covs, potential_outcomes = podata)
  
  analysis      <- declare_analysis(formula = Y ~ Z, treatment.variable = "Z", method = "lm")
  
  power         <- power(sims = 10, alpha = .05, analysis = analysis,
                         covs =  cov_dgp(c(1:10), rep(0:1,5), N_per_cluster = 4), 
                         betas = c(0,1),  ate=0,  block_var = "L1",
                         design = design)
  
  mock$Z        <- assign_treatment(design)
  mock$Y        <- observed_y(outcome = "Y", treatment.assignment = "Z", data = mock)
  
  M             <- run_analysis(analysis = analysis, data = mock)  
  
  ##pre_register(covs, podata, design, mock, analyze, power)
  pre_register(design = design, data = mock, analysis = analysis,
               registration_title = "Lady Tasting Tea", 
               registration_authors = c("Ronald A. Fisher"), 
               registration_abstract = "Description of the lady tasting tea experiment",
               random.seed = 42, dir = getwd(), type = "rmarkdown",
               make_output = TRUE, output_format = "pdf", keep_tex = TRUE, 
               open_output = TRUE)
  
  
})
