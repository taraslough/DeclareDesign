

#' Estimate the ICC of a given variable
#'
#' @param variable A vector whose intra-cluster correlation coefficient should be estimated
#' @param cluster A vector of cluster IDs indicating the cluster corresponding to each element of variable 
#' @export
calculate_ICC <- function(variable,cluster){
  # Code inspired by ICC package, Matthew Wolak <matthewwolak@gmail.com>
  # 2015-06-17 licensed under GPL (>= 2)
  anova_out <- anova(aov(variable ~ as.factor(cluster)))
  J <- length(unique(cluster))
  mean_sq_j <- anova_out$"Mean Sq"[1]
  mean_sq_i <- var_i <- anova_out$"Mean Sq"[2]
  n_j <- tapply(variable,as.factor(cluster), FUN = length)
  k <- (1/(J - 1)) * (sum(n_j) - (sum(n_j^2)/sum(n_j)))
  var_j <- (mean_sq_j - mean_sq_i)/k
  rho <- var_j/(var_i + var_j)
  return(rho)
}

#' @export
get_regression_coefficient <- function(model, formula = NULL, coefficient_name, 
                                       statistics = c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                       estimates_labels = ""){
  
  coef_num <- which(names(coef(model)) == coefficient_name)
  df <- df.residual(model)
  est <- coef(model)[coef_num]
  se <- sqrt(diag(vcov(model)))[coef_num]
  p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
  conf_int <- suppressMessages(confint(model))[coef_num, ]
  
  output <- matrix(c(est, se, p, conf_int, df), 
                   dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                   paste0(summary(model)$terms[[2]], "~", paste(all.vars(summary(model)$terms[[3]]), collapse = "+"), "_", estimates_labels)))
  
  return(output[which(rownames(output) %in% statistics), , drop = FALSE])
}

#' @export
difference_in_means <- function(formula, data, weights = NULL, subset = NULL, ##cluster_variable = NULL, 
                                alpha = .05) {
  
  if(length(all.vars(formula[[3]]))>1)
    stop("The formula should only include one variable on the right-hand side: the treatment variable.")
  
  d_i_m <- function(Y, t, ##w, 
                    cond1, cond2, alpha){
    N <- length(Y)
    diff <- mean(Y[t == cond1]) - mean(Y[t == cond2])
    se <- sqrt(var(Y[t == cond1])/sum(t == cond1) + var(Y[t == cond2])/sum(t == cond2))
    df <- N - 2
    p <- 2 * pt(abs(diff/se), df = df, lower.tail = FALSE)
    ci_lower <- diff - qt(1 - alpha/2, df = df) * se
    ci_upper <- diff + qt(1 - alpha/2, df = df) * se
    return(c(diff, se, p, ci_lower, ci_upper, df))
  }
  
  condition_names <- unique(data[,all.vars(formula[[3]])])
  combn <- combn(rev(sort(condition_names)), m = 2)
  combn_names <- apply(combn, 2, function(x) paste(x, collapse = "-"))
  
  if(!is.null(subset))
    data <- data[subset, ]
  
  ##if(!is.null(weights))
  ##  w <- weights[subset]
  
  ##if(!is.null(cluster_variable)){
  ##  data <- aggregate(data[,all.vars(formula)], list(d_i_m_cluster_var__ = data[,cluster_variable]), FUN = function(x) mean(x, na.rm = T))
  ##}
  
  Y <- data[, all.vars(formula[[2]])]
  t <- data[, all.vars(formula[[3]])]
  
  return_matrix <- matrix(NA, nrow = 6, ncol = ncol(combn), 
                          dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                          paste0(all.vars(formula[[2]]), "~", combn_names, 
                                                 "_diff_in_means")))
  for(c in 1:ncol(combn)){
    return_matrix[, c] <- d_i_m(Y = Y, t = t, ##w = w, 
                                cond1 = combn[1, c], cond2 = combn[2, c], alpha = alpha)
  }
  
  return(return_matrix)
}

#' @export
difference_in_means_blocked <- function(formula, data, subset = NULL, block_variable = NULL, ##cluster_variable = NULL, 
                                        alpha = .05) {
  
  if(is.null(block_variable))
    stop("This difference-in-means estimator can only be used if you specify block_variable, a string indicating which variable in the data frame contains the blocks.")
  
  if(length(all.vars(formula[[3]]))>1)
    stop("The formula should only include one variable on the right-hand side: the treatment variable.")
  
  d_i_m_blocked <- function(Y, t, b, cond1, cond2, alpha){
    
    N <- length(Y)
    
    block_names <- sort(unique(b))
    
    block_weights <- (sapply(block_names, function(i) sum(b==i)))/N
    
    means_by_block <- tapply(Y,list(t, b), mean, na.rm=TRUE)
    diff <- (block_weights %*% (means_by_block[as.character(cond1),] - means_by_block[as.character(cond2),]))
    
    vars <- sapply(block_names, function(i)  {
      var(Y[b==i & t == cond1], na.rm = TRUE )/sum(b==i & t == cond1)+
        var(Y[b==i & t == cond2], na.rm = TRUE )/sum(b==i & t == cond2)})
    se  <- (block_weights^2 %*% vars)^.5
    
    df <- length(Y) - length(block_names) - 1
    p <- 2 * pt(abs(diff/se), df = df, lower.tail = FALSE)
    ci_lower <- diff - qt(1 - alpha / 2, df = df) * se
    ci_upper <- diff + qt(1 - alpha / 2, df = df) * se
    
    return(c(diff, se, p, ci_lower, ci_upper, df))
    
  }
  
  condition_names <- unique(data[,all.vars(formula[[3]])])
  combn <- combn(rev(sort(condition_names)), m = 2)
  combn_names <- apply(combn, 2, function(x) paste(x, collapse = "-"))
  
  if(!is.null(subset))
    data <- data[subset, ]
  
  ##if(!is.null(cluster_variable)){
  ##  data <- aggregate(names(data)[!which(names(data) %in% cluster_variable)], list(d_i_m_cluster_var__ = data[,cluster_variable]), FUN = mean)
  ##}
  
  Y <- data[, all.vars(formula[[2]])]
  t <- data[, all.vars(formula[[3]])]
  b <- data[, block_variable]
  
  return_matrix <- matrix(NA, nrow = 6, ncol = ncol(combn), 
                          dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                          paste0(all.vars(formula[[2]]), "~", combn_names, 
                                                 "_diff_in_means")))
  for(c in 1:ncol(combn)){
    return_matrix[, c] <- d_i_m_blocked(Y = Y, t = t, b = b, cond1 = combn[1, c], cond2 = combn[2, c], alpha = alpha)
  }
  
  return(return_matrix)
}



# Diagnose summary functions ----------------------------------------------

#' @export
calculate_PATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]))
  
  if(class(SATE) == "matrix")
    PATE <- apply(SATE, 1, mean, na.rm = T)
  else
    PATE <- mean(SATE, na.rm = T)
  
  return(list(statistic = PATE, label = "PATE"))
}


#' @export
calculate_sd_SATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]))
  
  if(class(SATE) == "matrix")
    sd_SATE <- apply(SATE, 1, sd, na.rm = T)
  else
    sd_SATE <- sd(SATE, na.rm = T)
  
  return(list(statistic = sd_SATE, label = "sd(SATE)"))
}

#' @export
calculate_power <- function(estimates, ...){
  p <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["p", , drop = FALSE]))
  
  if(class(p) == "matrix")
    power <- apply(p < .05, 1, mean, na.rm = T)
  else
    power <- mean(p < .05, na.rm = T)
  
  return(list(statistic = power, label = "Power"))
}

#' @export
calculate_RMSE <- function(estimates, estimands, ...){
  error <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - 
                                                                estimands[[i]]))
  
  if(class(error) == "matrix")
    RMSE <- apply(error, 1, function(x) sqrt(mean(x^2, na.rm = T)))
  else
    RMSE <- sqrt(mean(error^2, na.rm = T))
  
  return(list(statistic = RMSE, label = "RMSE"))
}

#' @export
calculate_bias <- function(estimates, estimands, ...){
  
  PATE <- calculate_PATE(estimands = estimands)$statistic
  
  est_PATE_diff <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - PATE))
  
  if(class(est_PATE_diff) == "matrix")
    bias <- apply(est_PATE_diff, 1, mean, na.rm = T)
  else
    bias <- mean(est_PATE_diff, na.rm = T)
  
  return(list(statistic = bias, label = "Bias"))
}

#' @export
calculate_coverage <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)$statistic
  
  ci_covers_estimate <- sapply(1:length(estimates), function(i) as.numeric(PATE <= estimates[[i]]["ci_upper", , drop = FALSE] & 
                                                                             PATE >= estimates[[i]]["ci_lower", , drop = FALSE]))
  
  if(class(ci_covers_estimate) == "matrix")
    coverage <- apply(ci_covers_estimate, 1, mean, na.rm = T)
  else
    coverage <- mean(ci_covers_estimate, na.rm = T)
  
  return(list(statistic = coverage, label = "Coverage"))
}

#' @export
calculate_type_S_rate <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)$statistic
  
  sign_error <- sapply(1:length(estimates), function(i) as.numeric(sign(PATE) != sign(estimates[[i]]["est", , drop = FALSE])))
  
  if(class(sign_error) == "matrix")
    type_S_rate <- apply(sign_error, 1, mean, na.rm = T)
  else
    type_S_rate <- mean(sign_error, na.rm = T)
  
  return(list(statistic = type_S_rate, label = "Type S Rate"))
}

#' @export
calculate_exaggeration_ratio <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)$statistic
  
  exaggeration_ratio <- sapply(1:length(estimates), function(i) as.numeric(abs(estimates[[i]]["est", , drop = FALSE] / PATE)))
  
  if(class(exaggeration_ratio) == "matrix")
    mean_exaggeration_ratio <- apply(exaggeration_ratio, 1, mean, na.rm = T)
  else
    mean_exaggeration_ratio <- mean(exaggeration_ratio, na.rm = T)
  
  return(list(statistic = mean_exaggeration_ratio, label = "Mean Exagg. Ratio"))
}

