vcovCluster <- function(model, cluster){
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}



# Arguments to function ---------------------------------------------------

# N = population size
# n = sample size
# m = number of arms
# prob_assign = vector of units assigned to each arm (control first)
# mu_Y0 = expectation of untreated potential outcome
# ATEs = vector of ATEs for each treatment arm (length probability_each - 1)
# noise_scale = rescale the noise for each experimental group if desired
# coef_X = coefficient on covariate X
# location_scale_X = mean and SD of normally-distributed X covariate
# cov_adjustment = use covariate adjustment in estimation
# block_var_probs = distribution of multinomial blocking variable (probability per level)
# blocked_RA = use blocked random assignment
# block_probability_each = matrix of probabilities if different blocks have different probs of assignment to treatment
# n_clust_pop = number of clusters in population
# n_clust_samp = number of clusters in sample

m_arm_template <- function(N, 
                           n = N, 
                           m = 2,
                           probability_each = rep(1/m, m), 
                           mu_Y0 = 0, 
                           ATEs = 0, 
                           noise_scale = 1, 
                           coef_X = 0,
                           location_scale_X = c(0, 1),
                           cov_adjustment = F,
                           block_var_probs = c(.5, .5),
                           block_var_vector = NULL,
                           blocked_RA = F,
                           block_probability_each = NULL,
                           n_clust_pop = NULL,
                           clust_noise_coef = 1,
                           n_clust_samp = NULL,
                           robust_SEs = T){
  
  if(n > N){stop("Sample (n) cannot be larger than population (N)!")}
  if(m < 2){stop("Experiment requires that m > 1.")}
  if(length(ATEs) != (length(probability_each) - 1)) {stop("Error: Vector of ATEs is not of length probability_each-1!")}
  if(length(noise_scale) != m  &  length(noise_scale)!= 1)
  {stop("Error: Vector of noise scale must be of length m or 1!")}
  if(sum(noise_scale < 0) > 0){stop("Error: All noise_scale inputs must be positive!")}
  if(location_scale_X[2] <= 0){stop("Error: Standard deviation of X must be positive!")}
  if(length(coef_X)> 1 | !is.numeric(coef_X)){stop("Error: coef_X must be one number!")}
  if(sum(block_var_probs) != 1){stop("Block_var_probs must sum to 1!")}
  if(is.numeric(n_clust_pop) & is.numeric(n_clust_samp)){
    if(n_clust_pop< n_clust_samp){
      stop("Number of clusters in sample cannot exceed number of clusters in population!")}}
  if(!is.null(block_probability_each)){
    if(sum(rowSums(block_probability_each) != 1) != 0){
      stop("Rows of block_probability_each must sum to 1!")}}
  if(!is.null(block_var_vector)&length(block_var_vector)!=N){stop("Length of block variable vector must be equal to N!")}
  
  if(is.null(n_clust_pop)){  
    if(is.null(block_var_vector)){
    population    <- declare_population(
      noise = declare_variable(location_scale = c(0, 1)),
      X = declare_variable(location_scale = location_scale_X),
      block_var = declare_variable(type = "multinomial",
                                   probabilities = block_var_probs, outcome_categories = 1:length(block_var_probs)),
      size = N, super_population = T)
  }
  
  if(!is.null(block_var_vector)){
    population    <- declare_population(
      noise = declare_variable(location_scale = c(0, 1)),
      X = declare_variable(location_scale = location_scale_X),
      block_var = block_var_vector,
      size = N, super_population = T)
}
    write_PO_formula <- function(ATEs, noise_scale){
      scale <- ifelse(length(noise_scale) == 1, noise_scale, noise_scale[-1])
      tab <- cbind(ATEs, scale)
      
      POs <- paste0("Y ~ ", mu_Y0, " + ", noise_scale[1], " * noise * (Z == 'control') +", 
                    paste(sapply(1:nrow(tab), function(j) {
                      paste0(tab[j, 1], " * (Z == 'treatment",  j, "') + ", 
                             tab[j, 2], " * noise * (Z == 'treatment", j, "')")}), 
                      collapse = " + "), " + X * ", coef_X)
      
      return(POs)
    }
    
    
    potential_outcomes <- declare_potential_outcomes(
      condition_names = c("control", paste0("treatment", 1 : length(ATEs))),
      formula = as.formula(write_PO_formula(ATEs = ATEs, noise_scale = noise_scale)))
    
    if(N == n){
      sampling <- declare_sampling(sampling = F)
    }
    if(N != n){
      sampling <- declare_sampling(n = n)
    }
    
    if(!blocked_RA){
      assignment   <- declare_assignment(probability_each = probability_each, 
                                         potential_outcomes = potential_outcomes)
    }
    
    if(blocked_RA){
      assignment   <- declare_assignment(block_variable_name = "block_var",
                                         block_probabilities = block_probability_each,
                                         potential_outcomes = potential_outcomes)
    }
    estimand_function <- function(data, treatment_num){
      return(mean(data[,paste0("Y_Z_treatment", treatment_num)] - data$Y_Z_control))
    }
    
    if(!cov_adjustment){estimator_formula = "Y ~ Z"}
    if(cov_adjustment){estimator_formula = "Y ~ Z + X"}
    
    linear_estimator <- function(data, treatment_num){
      model <- lm(as.formula(estimator_formula), weights = Z_assignment_probabilities, data = data)
      if(!robust_SEs){
        return(get_regression_coefficient(model, coefficient_name = paste0("Ztreatment", treatment_num)))}
      if(robust_SEs){
        return(get_regression_coefficient_robust(model, coefficient_name = paste0("Ztreatment", treatment_num)))}  
    }
  }
  
  if(!is.null(n_clust_pop)){
    if(is.null(block_var_vector)){
    population    <- declare_population(
      individual = list(noise_ind = declare_variable(),
                        X = declare_variable(location_scale = location_scale_X)),
      cluster = list(noise_clust = declare_variable(),
                     block_var_clust = declare_variable(type = "multinomial",
                                                        probabilities = block_var_probs, 
                                                        outcome_categories = 1:length(block_var_probs))),
      size = c(N, n_clust_pop))}
    if(!is.null(block_var_vector)){
      population    <- declare_population(
        individual = list(noise_ind = declare_variable(),
                          X = declare_variable(location_scale = location_scale_X)),
        cluster = list(noise_clust = declare_variable(),
                       block_var_clust = block_var_vector),
        size = c(N, n_clust_pop))}
    
    
    write_PO_formula <- function(ATEs, noise_scale, clust_noise_coef){
      scale <- ifelse(length(noise_scale) == 1, noise_scale, noise_scale[-1])
      tab <- cbind(ATEs, scale)
      
      POs <- paste0("Y ~ ", mu_Y0, " + ", noise_scale[1], " * noise_ind * (Z == 'control') +", 
                    paste(sapply(1:nrow(tab), function(j) {
                      paste0(tab[j, 1], " * (Z == 'treatment",  j, "') + ", 
                             tab[j, 2], " * noise_ind * (Z == 'treatment", j, "')")}), 
                      collapse = " + "), " + X * ", coef_X, " + noise_clust * ", clust_noise_coef)
      
      return(POs)
    }
    
    potential_outcomes <- declare_potential_outcomes(
      condition_names = c("control", paste0("treatment", 1 : length(ATEs))),
      formula = as.formula(write_PO_formula(ATEs = ATEs, noise_scale = noise_scale, 
                                            clust_noise_coef = clust_noise_coef)))
    
    if(n_clust_pop == n_clust_samp){
      sampling <- declare_sampling(sampling = F)
    }
    
    if(n_clust_pop != n_clust_samp){
      sampling <- declare_sampling(n = n_clust_samp,
                                   cluster_variable_name = "cluster_ID")
    }
    
    
    if(!blocked_RA){
      assignment   <- declare_assignment(cluster_variable_name = "cluster_ID", 
                                         potential_outcomes = potential_outcomes)
    }
    
    if(blocked_RA){
      assignment   <- declare_assignment(block_variable_name = "block_var_clust",
                                         cluster_variable_name = "cluster_ID", 
                                         potential_outcomes = potential_outcomes)
    }
    
    
    estimand_function <- function(data, treatment_num){
      return(mean(data[,paste0("Y_Z_treatment", treatment_num)] - data$Y_Z_control))
    }
    
    if(!cov_adjustment){estimator_formula = "Y ~ Z"}
    if(cov_adjustment){estimator_formula = "Y ~ Z + X"}
    
    get_regression_coefficient_clustered <- function (model, 
                                                      formula = NULL, 
                                                      coefficient_name, 
                                                      statistics = c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                                      label = ""){ 
      require(sandwich)
      coef_num <- which(names(coef(model)) %in% coefficient_name)
      df <- df.residual(model)
      est <- coef(model)[coef_num]
      se <- sqrt(diag(vcovCluster(model = model, cluster = data$cluster_ID)))[coef_num]
      p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
      conf_int <- est + se %o% qt(c(0.025, 0.975), summary(model)$df[2])
      output <- matrix(c(est, se, p, conf_int, df), 
                       dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                       paste0(summary(model)$terms[[2]], 
                                              "~", paste(all.vars(summary(model)$terms[[3]]), collapse = "+"), 
                                              "_", label)))
      return(output[which(rownames(output) %in% statistics), , drop = FALSE])
    }
    
    linear_estimator <- function(data, treatment_num){
      model <- lm(as.formula(estimator_formula), weights = Z_assignment_probabilities, data = data)
      return(get_regression_coefficient_clustered(model, coefficient_name = paste0("Ztreatment", treatment_num)))
    }
    
  }
  
  
  make_estimator <- function(treatment_num){
    estimand  <- declare_estimand(estimand_function  = estimand_function, 
                                  treatment_num = treatment_num,
                                  potential_outcomes = potential_outcomes)
    
    estimator <- declare_estimator(estimates = linear_estimator, 
                                   treatment_num = treatment_num, 
                                   estimand = estimand,
                                   labels = paste0("ATE_hat_treatment", treatment_num))
    return(estimator)
  }
  
  
  
  estimator_list <- lapply(X = 1 : length(ATEs), FUN = make_estimator)
  
  
  my_design <- declare_design(
    population         = population, 
    potential_outcomes = potential_outcomes, 
    sampling           = sampling, 
    assignment         = assignment, 
    estimator          = estimator_list)
  
  return(my_design)
}
