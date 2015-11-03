cluster_function_generic <- function(clusters_internal, cluster_name, sample){
  x <- sample[,clusters_internal]
  n_digits <- nchar(as.character(length(unique(x))))
  x <- paste0("cluster_",sprintf(paste0("%0",n_digits,"d"),(as.numeric(as.factor(x)))))
  cluster_df <- data.frame(x, stringsAsFactors = FALSE)
  colnames(cluster_df) <- cluster_name
  return(cluster_df)
} 


blocks_function_generic <- function(blocks_internal, block_name, sample){
  x <- sample[,blocks_internal]
  n_digits <- nchar(as.character(length(unique(x))))
  x <- paste0("block_",sprintf(paste0("%0",n_digits,"d"),(as.numeric(as.factor(x)))))
  blocks_df <- data.frame(x, stringsAsFactors = FALSE)
  colnames(blocks_df) <- block_name
  return(blocks_df)
} 

recode_function_generic <- function(blocks_untransformed, block_count){
  blocks_transformed <- cut(x = blocks_untransformed, 
                            breaks = quantile(x = blocks_untransformed, 
                                              probs = seq(from = 0, to = 1, 
                                                          length.out = block_count + 1)),
                            include.lowest = TRUE)
  return(blocks_transformed)
}

multi_blocks_function_generic <- function(blocks, sample, block_name = block_name, block_count = block_count, recode_function = recode_function){
  
  df <- model.matrix(as.formula(paste("~", paste(blocks, collapse = "+"))), data = sample)
  pca_first_component <- summary(princomp(df))$scores[,1]
  
  if(length(unique(pca_first_component)) < block_count)
    warning(paste("Not enough variation to make", block_count, 
                  "blocks. We made", length(unique(pca_first_component)), "blocks instead."))
  
  if(length(unique(pca_first_component)) > block_count){
    blocks_transformed <- recode_function(blocks_untransformed = 
                                            pca_first_component, block_count = block_count)
  }  else {
    blocks_transformed <- pca_first_component
  }
  
  n_digits <- nchar(as.character(length(unique(blocks_transformed))))
  blocks_transformed <- paste0("block_",sprintf(paste0("%0",n_digits,"d"),
                                                (as.numeric(as.factor(blocks_transformed)))))
  blocks_df <- data.frame(blocks_transformed, stringsAsFactors = FALSE)
  colnames(blocks_df) <- block_name
  
  return(blocks_df)
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
  
  return(PATE)
}


#' @export
calculate_sd_SATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]))
  
  if(class(SATE) == "matrix")
    sd_SATE <- apply(SATE, 1, sd, na.rm = T)
  else
    sd_SATE <- sd(SATE, na.rm = T)
  
  return(sd_SATE)
}

#' @export
calculate_power <- function(estimates, ...){
  p <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["p", , drop = FALSE]))
  
  if(class(p) == "matrix")
    power <- apply(p < .05, 1, mean, na.rm = T)
  else
    power <- mean(p < .05, na.rm = T)
  
  return(power)
}

#' @export
calculate_RMSE <- function(estimates, estimands, ...){
  error <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - 
                                                                estimands[[i]]))
  
  if(class(error) == "matrix")
    RMSE <- apply(error, 1, function(x) sqrt(mean(x^2, na.rm = T)))
  else
    RMSE <- sqrt(mean(error^2, na.rm = T))
  
  return(RMSE)
}

#' @export
calculate_bias <- function(estimates, estimands, ...){
  
  PATE <- calculate_PATE(estimands = estimands)
  
  est_PATE_diff <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - PATE))
  
  if(class(est_PATE_diff) == "matrix")
    bias <- apply(est_PATE_diff, 1, mean, na.rm = T)
  else
    bias <- mean(est_PATE_diff, na.rm = T)
  
  return(bias)
}

#' @export
calculate_coverage <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)
  
  ci_covers_estimate <- sapply(1:length(estimates), function(i) as.numeric(PATE <= estimates[[i]]["ci_upper", , drop = FALSE] & 
                                                                             PATE >= estimates[[i]]["ci_lower", , drop = FALSE]))
  
  if(class(ci_covers_estimate) == "matrix")
    coverage <- apply(ci_covers_estimate, 1, mean, na.rm = T)
  else
    coverage <- mean(ci_covers_estimate, na.rm = T)
  
  return(coverage)
}

#' @export
calculate_type_S_rate <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)
  
  sign_error <- sapply(1:length(estimates), function(i) as.numeric(sign(PATE) != sign(estimates[[i]]["est", , drop = FALSE])))
  
  if(class(sign_error) == "matrix")
    type_S_rate <- apply(sign_error, 1, mean, na.rm = T)
  else
    type_S_rate <- mean(sign_error, na.rm = T)
  
  return(type_S_rate)
}

#' @export
calculate_exaggeration_ratio <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)
  
  exageration_ratio <- sapply(1:length(estimates), function(i) as.numeric(abs(estimates[[i]]["est", , drop = FALSE] / PATE)))
  
  if(class(exageration_ratio) == "matrix")
    mean_exageration_ratio <- apply(exageration_ratio, 1, mean, na.rm = T)
  else
    mean_exageration_ratio <- mean(exageration_ratio, na.rm = T)
  
  return(mean_exageration_ratio)
}

