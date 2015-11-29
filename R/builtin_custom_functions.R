#' Estimate the ICC of a given variable
#'
#' @param variable A vector whose intra-cluster correlation coefficient should be estimated
#' @param cluster_variable A vector of cluster IDs indicating the cluster corresponding to each element of variable 
#' @export
calculate_ICC <- function(variable, cluster_variable){
  # Code inspired by ICC package, Matthew Wolak <matthewwolak@gmail.com>
  # 2015-06-17 licensed under GPL (>= 2)
  anova_out <- anova(aov(variable ~ as.factor(cluster_variable)))
  J <- length(unique(cluster_variable))
  mean_sq_j <- anova_out$"Mean Sq"[1]
  mean_sq_i <- var_i <- anova_out$"Mean Sq"[2]
  n_j <- tapply(variable,as.factor(cluster_variable), FUN = length)
  k <- (1/(J - 1)) * (sum(n_j) - (sum(n_j^2)/sum(n_j)))
  var_j <- (mean_sq_j - mean_sq_i)/k
  rho <- var_j/(var_i + var_j)
  return(rho)
}


# Default transform function ----------------------------------------------

default_transform_function <- function(data, options, assignment_variable_name){
  if(!is.null(options)){
    for(i in 1:length(options)){
      data[, names(options)[i]] <- as.numeric(data[, assignment_variable_name] %in% options[[i]])
    }
  }
  return(data)
}


# Built-in Estimates Functions --------------------------------------------


#' Extract Regression Coefficients
#' 
#' @param model A model fit
#' @param formula An optional formula
#' @param coefficient_name The name of the coefficient to extract.
#' @param statistics The statistics to extract. Defaults to c("est", "se", "p", "ci_lower", "ci_upper", "df")
#' @param label Character label for the regression coefficient
#'
#' @export
get_regression_coefficient <- function(model, formula = NULL, coefficient_name, 
                                       statistics = c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                       label = ""){
  
  coef_num <- which(names(coef(model)) %in% coefficient_name)
  df <- df.residual(model)
  est <- coef(model)[coef_num]
  se <- sqrt(diag(vcov(model)))[coef_num]
  p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
  conf_int <- suppressMessages(confint(model))[coef_num, ]
  
  output <- matrix(c(est, se, p, conf_int, df), 
                   dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                   paste0(summary(model)$terms[[2]], "~", paste(all.vars(summary(model)$terms[[3]]), collapse = "+"), "_", label)))
  
  return(output[which(rownames(output) %in% statistics), , drop = FALSE])
}


# Built-in-Estimators -----------------------------------------------------

#' Built-in Estimators: Difference-in-means
#' 
#' @param formula An object of class "formula", such as Y ~ Z
#' @param data A data.frame, often created by \code{\link{draw_population}}.
#' @param weights An optional vector of weights (not yet implemented).
#' @param subset An optional vector specifying a subset of observations to be used.
#' @param alpha The significance level, 0.05 by default.
#'
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


#' Built-in Estimators: Blocked Difference-in-means 
#' 
#' @param formula An object of class "formula", such as Y ~ Z
#' @param data A data.frame, often created by \code{\link{draw_population}}.
#' @param weights An optional vector of weights (not yet implemented).
#' @param subset An optional vector specifying a subset of observations to be used.
#' @param block_variable_name The name of the blocking variable.
#' @param alpha The significance level, 0.05 by default.
#'
#' @export
difference_in_means_blocked <- function(formula, data, weights = NULL, subset = NULL, 
                                        block_variable_name = NULL, ##cluster_variable = NULL, 
                                        alpha = .05) {
  
  if(is.null(block_variable_name))
    stop("This difference-in-means estimator can only be used if you specify block_variable_name, a string indicating which variable in the data frame contains the blocks.")
  
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
  b <- data[, block_variable_name]
  
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

summary_internal <- function(object, statistic, na.rm = T, ...){
  if(class(object) == "matrix"){
    return(apply(object, 1, statistic, na.rm = na.rm, ... = ...))
  } else {
    return(statistic(object, na.rm = na.rm, ... = ...))
  }
}

get_estimand_internal <- function(estimands, ...){
  return(sapply(1:length(estimands), function(i) as.numeric(estimands[[i]])))
}

get_estimate_internal <- function(estimates, statistic, ...){
  est <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]][statistic, , drop = FALSE]))
  if(class(est) != "matrix"){
    est <- matrix(est, nrow = 1, ncol = length(est))
  }
  return(est)
}

calculate_mean_PATE <- function(population_estimands, ...){
  return(list(statistic = summary_internal(get_estimand_internal(population_estimands), mean),
              label = "Mean, PATE"))
}

calculate_sd_PATE <- function(population_estimands, ...){
  return(list(statistic = summary_internal(get_estimand_internal(population_estimands), sd),
              label = "S.D., PATE"))
}

calculate_mean_SATE <- function(sample_estimands, ...){
  SATEs <- sapply(1:length(sample_estimands), function(i) summary_internal(get_estimand_internal(sample_estimands[[i]]), mean))
  return(list(statistic = summary_internal(SATEs, mean),
              label = "Mean, SATE"))
}

calculate_sd_SATE <- function(sample_estimands, ...){
  SATEs <- sapply(1:length(sample_estimands), function(i) summary_internal(get_estimand_internal(sample_estimands[[i]]), mean))
  return(list(statistic = summary_internal(SATEs, sd),
              label = "S.D., SATE"))
}

calculate_mean_power <- function(estimates, ...){
  power <- sapply(1:length(estimates), function(i) summary_internal(get_estimate_internal(estimates[[i]], "p") < .05, mean))
  
  return(list(statistic = summary_internal(power, mean),
              label = "Mean, Power"))
}

calculate_sd_power <- function(estimates, ...){
  power <- sapply(1:length(estimates), function(i) summary_internal(get_estimate_internal(estimates[[i]], "p") < .05, mean))
  
  return(list(statistic = summary_internal(power, sd), 
              label = "S.D., Power"))
}

calculate_superpopulation_RMSE <- function(estimates, population_estimands, ...){
  PATE <- calculate_mean_PATE(population_estimands)$statistic
  error <- do.call(cbind, lapply(1:length(estimates), function(i) get_estimate_internal(estimates[[i]], "est") - PATE))
  
  return(list(statistic = summary_internal(error, function(x, na.rm = T) sqrt(mean(x^2))),
              label = "Super-Population RMSE"))               
  
}

calculate_population_RMSE <- function(estimates, population_estimands, ...){
  RMSE <- sapply(1:length(estimates), function(i) summary_internal(get_estimate_internal(estimates[[i]], "est") - get_estimand_internal(population_estimands[[i]]), function(x, na.rm = T) sqrt(mean(x^2))))
  
  return(list(statistic = summary_internal(RMSE, mean),
              label = "Mean Population RMSE"))               
  
}

calculate_sample_RMSE <- function(estimates, sample_estimands, ...){
  RMSE <- sapply(1:length(estimates), function(i) summary_internal(get_estimate_internal(estimates[[i]], "est") - get_estimand_internal(sample_estimands[[i]]), function(x, na.rm = T) sqrt(mean(x^2))))
  
  return(list(statistic = summary_internal(RMSE, mean),
              label = "Mean Sample RMSE"))           
  
}

calculate_superpopulation_bias <- function(estimates, population_estimands, ...){
  PATE <- calculate_mean_PATE(population_estimands)$statistic
  bias <- do.call(cbind, lapply(1:length(estimates), function(i) get_estimate_internal(estimates[[i]], "est") - PATE))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Super-Population Bias"))               
  
}

calculate_population_bias <- function(estimates, population_estimands, ...){
  bias <- sapply(1:length(estimates), function(i) summary_internal(get_estimate_internal(estimates[[i]], "est") - get_estimand_internal(population_estimands[[i]]), mean))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean Population Bias"))               
  
}

calculate_sample_bias <- function(estimates, sample_estimands, ...){
  bias <- sapply(1:length(estimates), function(i) summary_internal(get_estimate_internal(estimates[[i]], "est") - get_estimand_internal(sample_estimands[[i]]), mean))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean Sample Bias"))           
  
}

calculate_superpopulation_coverage <- function(estimates, population_estimands, ...){
  PATE <- calculate_mean_PATE(population_estimands)$statistic
  ci_covers_estimate <- do.call(cbind, lapply(1:length(estimates), function(i) PATE <= get_estimate_internal(estimates[[i]], "ci_upper") 
                                              & PATE >= get_estimate_internal(estimates[[i]], "ci_lower") ))
  
  return(list(statistic = summary_internal(ci_covers_estimate, mean),
              label = "Super-Population Coverage"))               
  
}

calculate_population_coverage <- function(estimates, population_estimands, ...){
  ci_covers_estimate <- sapply(1:length(estimates), function(i) summary_internal(get_estimand_internal(population_estimands[[i]]) <= get_estimate_internal(estimates[[i]], "ci_upper") & get_estimand_internal(population_estimands[[i]]) >= get_estimate_internal(estimates[[i]], "ci_lower"), mean))
  
  return(list(statistic = summary_internal(ci_covers_estimate, mean),
              label = "Mean Population Coverage"))               
  
}

calculate_sample_coverage <- function(estimates, sample_estimands, ...){
  ci_covers_estimate <- sapply(1:length(estimates), function(i) summary_internal(get_estimand_internal(sample_estimands[[i]]) <= get_estimate_internal(estimates[[i]], "ci_upper") & get_estimand_internal(sample_estimands[[i]]) >= get_estimate_internal(estimates[[i]], "ci_lower"), mean))
  
  return(list(statistic = summary_internal(ci_covers_estimate, mean),
              label = "Mean Sample Coverage"))               
  
}

calculate_superpopulation_type_S_rate <- function(estimates, population_estimands, ...){
  PATE <- calculate_mean_PATE(population_estimands)$statistic
  bias <- do.call(cbind, lapply(1:length(estimates), function(i) sign(get_estimate_internal(estimates[[i]], "est")) != sign(PATE)))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Super-Population Type S Rate"))               
  
}

calculate_population_type_S_rate <- function(estimates, population_estimands, ...){
  bias <- sapply(1:length(estimates), function(i) summary_internal(sign(get_estimate_internal(estimates[[i]], "est")) != sign(get_estimand_internal(population_estimands[[i]])), mean))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean Population Type S Rate"))               
  
}

calculate_sample_type_S_rate <- function(estimates, sample_estimands, ...){
  bias <- sapply(1:length(estimates), function(i) summary_internal(sign(get_estimate_internal(estimates[[i]], "est")) != sign(get_estimand_internal(sample_estimands[[i]])), mean))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean Sample Type S Rate"))           
  
}

calculate_superpopulation_exaggeration_ratio <- function(estimates, population_estimands, ...){
  PATE <- calculate_mean_PATE(population_estimands)$statistic
  bias <- do.call(cbind, lapply(1:length(estimates), function(i) get_estimate_internal(estimates[[i]], "est") / PATE))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Super-Population Exaggeration Ratio"))               
  
}

calculate_population_exaggeration_ratio <- function(estimates, population_estimands, ...){
  bias <- sapply(1:length(estimates), function(i) summary_internal( get_estimate_internal(estimates[[i]], "est") != get_estimand_internal(population_estimands[[i]]), mean))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean Population Exaggeration Ratio"))               
  
}

calculate_sample_exaggeration_ratio <- function(estimates, sample_estimands, ...){
  bias <- sapply(1:length(estimates), function(i) summary_internal( get_estimate_internal(estimates[[i]], "est") / get_estimand_internal(sample_estimands[[i]]), mean))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean Sample Exaggeration Ratio"))           
  
}

calculate_mean_estimate <- function(estimates, ...){
  bias <- do.call(cbind, lapply(1:length(estimates), function(i) get_estimate_internal(estimates[[i]], "est")))
  
  return(list(statistic = summary_internal(bias, mean),
              label = "Mean, Estimate"))               
  
}

calculate_sd_estimate <- function(estimates, ...){
  bias <- do.call(cbind, lapply(1:length(estimates), function(i) get_estimate_internal(estimates[[i]], "est")))
  
  return(list(statistic = summary_internal(bias, sd),
              label = "S.D., Estimate"))               
  
}


# Potential outcomes ------------------------------------------------------

#' Default potential outcomes function
#' 
#' @param formula A formula describing the outcomes as a function of a 
#' @param data 
#'
#' @export
default_potential_outcomes_function <- function(formula, data){
  
  return(eval(expr = formula[[3]], envir = data))
  
}

#' @export
proportion_potential_outcomes_function <- function(population_proportions = .5, data){
  
  N <- nrow(data)
  
  is_scalar <- is.numeric(population_proportions) & 
    length(population_proportions) == 1 
  
  if(is_scalar){
    population_proportions <- matrix(
      data = c(1-population_proportions,
               population_proportions),
      byrow = T,
      nrow = 2)
    con_names <- c(0,1)
  }
  
  counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
  
  row_names <- rownames(population_proportions)
  
  if(is.null(row_names)){
    if(is.null(con_names)){
      con_names <- 1:nrow(population_proportions)
    }
  } else {
    con_names <- row_names
  }
  
  outcomes <- apply(counts, 2, function(times){
    sample(
      rep(con_names,times = times)
    )
  })
  
  colnames(outcomes) <- colnames(population_proportions)
  
  return(outcomes)
  
}




# Bootstrap ---------------------------------------------------------------


#' Bootstrap data, including multi-level data
#' 
#' @param data A data.frame, usually provided by the user and not created with the DeclareDesign package.
#' @param N The number of units in the resulting bootstrapped data.frame.
#' @param N_per_level The number of units in each level of the bootstrapped heirarchy.
#' @param group_sizes_per_level A list of group sizes per level in the heirarchy.
#' @param level_ID_variables A vector of variable names, indicating the variables that desibe the heirarchy.
#' 
#' @return A data.frame
#'
#' @export
bootstrap_data <- function(data, size, level_IDs = NULL){
  
  hierarchy <- get_hierarchy(size = size)
  N <- hierarchy$N
  N_per_level <- hierarchy$N_per_level
  group_sizes_per_level <- hierarchy$group_sizes_per_level
  N_levels <- hierarchy$N_levels
  
  sample_by_level <- list()
  for(j in N_levels:1){
    if(j == N_levels){
      if(is.null(level_IDs) & N_levels==1){
        sample_by_level[[j]] <- sample(1:nrow(data), N_per_level[j], replace = TRUE)
      }
      sample_by_level[[j]] <- sample(data[, level_IDs[j]], N_per_level[j], replace = TRUE)
    } else {
      ## now go through each of the units in the level above it
      sample_current_level <- c()
      for(k in sample_by_level[[j+1]]){
        sample_current_level <- c(sample_current_level, 
                                  sample(data[data[, level_IDs[j+1]] == k, level_IDs[j]], 
                                         round(N_per_level[j]/N_per_level[j+1]), replace = TRUE))
      }
      sample_by_level[[j]] <- sample_current_level
    }
  }
  data <- data[sample_by_level[[1]], , drop = FALSE]
  
  ## reset row names so they are unique
  rownames(data) <- 1:nrow(data)
  
  return(data)
}





#' @export
make_list <- function(...) list(...)



#' @export
remaindr <- function(numerator,denominator) {
  m_each <- rep(numerator %/% denominator, denominator)
  remainder <- numerator %% denominator
  m_each <-
    m_each + ifelse(1:denominator %in% sample(1:denominator, remainder), 1, 0)
  return(m_each)
}



