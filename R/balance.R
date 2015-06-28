

#' @export
balance <- function(covariates, outcome = "Y", treatment_assignment = "Z", data, na.rm = TRUE){
  
  condition_names <- unique(data[,treatment_assignment])
  
  summ <- function(x) c(mean(x), quantile(x, .025), quantile(x, .05), quantile(.95), quantile(.975))
  ##stdize <- function(x) (x - mean(x))/sd(x)
  
  ## convert factors to numeric
  for(cov in covariates){
    if(class(data[,cov]) != "numeric")
      data[,cov] <- as.numeric(as.character(data[,cov]))
  }
  
  summ <- list()
  for(cond in condition_names){
    summ[[paste0("mean_", cond)]] <- apply(data[data[, treatment_assignment] == cond, covariates, drop = FALSE], 2, mean, na.rm = na.rm)
    summ[[paste0("sd_", cond)]] <- apply(data[data[, treatment_assignment] == cond, covariates, drop = FALSE], 2, sd, na.rm = na.rm)
  }
  
  summ <- data.frame(do.call(cbind, summ))
  
  combn <- combn(rev(condition_names), m = 2)
  
  for(i in 1:ncol(combn))
    summ[, paste0("diff_", paste(combn[,i], collapse = "-"))] <- summ[, paste0("mean_", combn[1, i])] - summ[, paste0("mean_", combn[2, i])]
  
  return_object <- list(summary = summ, condition_names = condition_names)

  structure(return_object, class = "balance")
  
}

#' @export
plot.balance <- function(x, covariate_labels = NULL,
                         zero_line = TRUE, zero_line_lty = "dotted", ...){
  summ <- table.balance(x)
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
table <- function(x) 
  UseMethod("table")

#' @export
table.balance <- function(x, ...){
  toupper_first <- function(x) {
    paste(toupper(substring(x, 1,1)), substring(x, 2), sep="")
  }
  colnames(x$summary) <- gsub("Sd", "Std. dev.", gsub("Diff", "Difference", gsub("_", " ", toupper_first(colnames(x$summary)))))
  return(x$summary)
}

