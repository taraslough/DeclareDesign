
N <- 1000
Y0 <- rbinom(N, 1, .4)
Y1 <- rbinom(N, 1, .6)

po_matrix <- data.frame(Y0, Y1)


design_1 <- declare_design(N = 1000, m = 500)

Z <- design_1$ra_fun()

reveal_pos <- function(po_matrix, Z){
  condition_names <- sort(unique(Z))
  if(ncol(po_matrix) != length(condition_names)){
    stop("Potential outcomes matrix (po_matrix) must have the same number of columns as unique conditions in random assignment vector (Z).")
  }
  if(nrow(po_matrix) != length(Z)){
    stop("Potential outcomes matrix (po_matrix) must have the same number of rows as the length of the random assignment vector (Z)")
  }
  
  Y_obs <- rep(NA, length(Z))
  
  for(i in 1:length(condition_names)){
    Y_obs[Z==condition_names[i]] <- po_matrix[Z==condition_names[i], i]
  }
  return(Y_obs)
}

Y_obs_test <- reveal_pos(po_matrix = po_matrix, Z = Z)


get_power <- function(data, design, po_matrix, test_fun, sims=1000){
  sims_vec <- rep(NA, sims)
  for(i in 1:sims){
    Z_sim <- design$ra_fun()
    Y_sim <- reveal_pos(po_matrix = po_matrix, Z = Z_sim)
    sims_vec[i] <- test_fun(Y = Y_sim, Z=Z_sim, data=data)
  }
  return(power=mean(sims_vec))
}

# series of test statistics

test_coefficients <- function(formula, which_coefs, data){
  
}
  
  
  
