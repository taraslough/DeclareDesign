#' Calculate Covariate Balance
#'
#' Description
#' @param covariates A character vector of covariate names to be used in the balance assessment.
#' @param treatment_assignment The name of the treatment assignment variable in data
#' @param assignment A assignment object, created by \code{\link{declare_assignment}}.
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @param report_difference what is it?
#' @param na.rm what is it?
#' @return A balance object
#' @examples
#' ##examples go here.
#' @export
get_balance <- function(covariates, treatment_assignment = "Z", assignment, data, 
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

#' @importFrom xtable xtable
#' @export
xtable.balance <- function(x, ...){
  xtable(x$summary, ... = ...)
}


