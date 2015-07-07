

#' @export
balance <- function(covariates, outcome = "Y", treatment_assignment = "Z", design, data, 
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
    statistic_labels <- c(statistic_labels, paste0("Mean, ", cond), paste0("S.D., ", cond))
    summ[[paste0("mean_", cond)]] <- apply(data[data[, treatment_assignment] == cond, covariates, drop = FALSE], 2, mean, na.rm = na.rm)
    summ[[paste0("sd_", cond)]] <- apply(data[data[, treatment_assignment] == cond, covariates, drop = FALSE], 2, sd, na.rm = na.rm)
  }
  
  summ <- data.frame(do.call(cbind, summ))
  
  if(report_difference == TRUE) {
    combn <- combn(rev(condition_names), m = 2)
    
    for(i in 1:ncol(combn)){
      statistic_labels <- c(statistic_labels, paste0("Std. Difference, ", paste(combn[,i], collapse = " - ")))
      summ[, paste0("diff_std_", paste(combn[,i], collapse = "-"))] <- (summ[, paste0("mean_", combn[1, i])] - summ[, paste0("mean_", combn[2, i])]) / summ[, "sd"]
    }
  }
  
  structure(list(summary = summ[, 3:ncol(summ)], condition_names = condition_names, 
                 statistic_labels = statistic_labels), 
            class = "balance")
  
}

#' @export
plot.balance <- function(x, covariate_labels = NULL,
                         zero_line = TRUE, zero_line_lty = "dotted", ...){
  summ <- x$summary
  colnames(summ) <- x$statistic_labels
  
  differences <- colnames(summ)[substr(colnames(summ), 1, 4) == "Diff"]
  par(mfrow = c(length(differences), 1), mar = c(4.25, 8, 0, 1))
  for(d in differences){
    if(!exists("xlim"))
      xlim <- c(min(0, summ[, differences]), max(0, summ[, differences]))
    if(!exists("xlab"))
      xlab <- colnames(summ[, d, drop = FALSE])
    plot(c(0,1), type = "n", xlim = xlim, ylim = c(1 - .1, nrow(summ) + .1), 
         axes = F, xlab = xlab, ylab = "", ...)
    if(zero_line == TRUE)
      abline(v = 0, lty = zero_line_lty)
    points(summ[, d], 1:nrow(summ), pch = 19)
    if(is.null(covariate_labels))
      covariate_labels <- rownames(summ)
    axis(2, at = 1:nrow(summ), labels = covariate_labels, tick = FALSE, las = 1)
    axis(1)
  }
}

#' @export
print.balance <- function(x, ...){
  colnames(x$summary) <- x$statistic_labels
  print(x$summary)
}

