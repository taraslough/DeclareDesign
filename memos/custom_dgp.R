make_clustered_data <- function(J = 10, n = 100, treatment_effect = .25, ICC = .1){
  ## Inspired by Mathieu et al, 2012, Journal of Applied Psychology
  if (J %% 2 != 0 | n %% 2 !=0) {
    stop(paste("Number of clusters (J) and size of clusters (n) must be even."))
  }
  Y0_j         <- rnorm(J,0,sd = (1 + treatment_effect) ^ 2 * sqrt(ICC))
  fake_data    <- expand.grid(i = 1:n,j = 1:J)
  fake_data$Y0 <- rnorm(n * J,0,sd = (1 + treatment_effect) ^ 2 * sqrt(1 - ICC)) + Y0_j[fake_data$j]
  fake_data$Y1 <- with(fake_data,mean(Y0) + treatment_effect + (Y0 - mean(Y0)) * (2 / 3))
  fake_data$Z  <- ifelse(fake_data$j %in% sample(1:J,J / 2) == TRUE, 1, 0)
  fake_data$Y  <- with(fake_data, Z * Y1 + (1 - Z) * Y0)
  return(fake_data)
}

set.seed(12345)
pretend_data <- make_clustered_data(J = 10,n = 100,treatment_effect = .25,ICC = .1)