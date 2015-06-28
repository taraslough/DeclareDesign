

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
  
  return(list(summary = summ, condition_names = condition_names))
}

#' @export
plot.balance <- function(x){
  summ <- table.balance(x)
  differences <- colnames(summ)[substr(colnames(summ), 1, 4) == "Diff"]
  plot(c(0,1), type = "n", xlim = c(-1, 1), ylim = c(1 - .1, nrow(summ) + .1), axes = F, xlab = "Difference", ylab = "")
  abline(v = 0, lty = "dotted")
  for(d in differences)
    points(summ[, d], 1:nrow(summ), pch = 19)
  axis(2, at = summ[, differences], labels = colnames(summ[, differences, drop = FALSE]), tick = FALSE)
  axis(1)
}

table.balance <- function(x){
  toupper_first <- function(x) {
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="")
  }
  colnames(x$summ) <- gsub("Sd", "Std. dev.", gsub("Diff", "Difference", gsub("_", " ", toupper_first(colnames(x$summ)))))
  return(x$summ)
}

