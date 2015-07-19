

#' @export
get_balance <- function(covariates, outcome = "Y", treatment_assignment = "Z", design, data, 
                    report_difference = FALSE, na.rm = TRUE){
  
  if(!any(colnames(data) %in% treatment_assignment))
    stop("The treatment variable defined in the argument treatment_assignment,", treatment_assignment, ", cannot be found in the data frame given in the data argument. Please make sure to assign treatment before running balance on this dataset.")
  
  condition_names <- unique(data[,treatment_assignment])
  
  summ <- function(x) c(mean(x), quantile(x, .025), quantile(x, .05), quantile(.95), quantile(.975))
  ##stdize <- function(x) (x - mean(x))/sd(x)
  
  ## convert factors to numeric 
  for(cov in covariates){
    if(class(data[,cov]) != "numeric")
      data[,cov] <- as.numeric(as.character(data[,cov]))
  }
  
  statistic_labels <- c()
  
  summ <- list()
  summ[[paste("mean")]] <- apply(data[, covariates, drop = FALSE], 2, mean, na.rm = na.rm)
  summ[[paste("sd")]] <- apply(data[, covariates, drop = FALSE], 2, mean, na.rm = na.rm)
  for(cond in condition_names){
    summ[[paste0("Mean, ", cond)]] <- apply(data[data[, treatment_assignment] == cond, covariates, drop = FALSE], 2, mean, na.rm = na.rm)
    summ[[paste0("S.D., ", cond)]] <- apply(data[data[, treatment_assignment] == cond, covariates, drop = FALSE], 2, sd, na.rm = na.rm)
  }
  
  summ <- do.call(cbind, summ)
  
  if(report_difference == TRUE) {
    combn <- combn(rev(condition_names), m = 2)
    
    for(i in 1:ncol(combn)){
      summ[, paste0("Std. Difference, ", paste(combn[,i], collapse = "-"))] <- (summ[, paste0("mean_", combn[1, i])] - summ[, paste0("mean_", combn[2, i])]) / summ[, "sd"]
    }
  }
  
  structure(list(summary = summ[, 3:ncol(summ), drop = FALSE], condition_names = condition_names, 
                 statistic_labels = statistic_labels), 
            class = c("balance"))
  
}

#' @export
print.balance <- function(x, ...){
  print(x$summary)
}

#' @export
xtable.balance <- function(x, ...){
  xtable(x$summary, ... = ...)
}


