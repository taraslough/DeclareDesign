#' Declare an experimental analysis
#'
#' Description
#' @param formula A standard R formula as a string or formula object, i.e. Y ~ Z, that indicates the outcome and the treatment indicator as well as covariates, block indicators, etc. that are used to construct estimates and/or estimands. By default, the formula provided is used for estimates and estimands.
#' @param treatment_variable The variable name of the treatment indicator as a string. Defaults to "Z".
#' @param outcome_variable The variable name of the outcome variable as a string. Defaults to "Y".
#' @param method The method used in the analysis to construct estimates and, by default, estimands. Indicate either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm. Defaults to "lm".
#' @param subset A string indicating the subset of the data to take in estimates and, by default, estimands.
#' @param weights A string indicating the name of the weights variable to be used in weighted estimators such as WLS.
#' @param estimand defaults to "ATE".
#' @param formula_estimand desc
#' @param method_estimand either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm
#' @param subset_estimand A string indicating the subset of the data to take for estimands, if the user wishes to have a different subset used in calculating estimands than the subset used for the estimates.
#' @param weights_estimand A string indicating the name of the weights variable to be used in weighted estimators such as WLS for calculating estimands only, if the user desires a different weights variable than used to calculate estimates.
#' @param qoi defaults to "ATE".
#' @param qoi_only desc
#' @param qoi_labels desc
#' @return a list containing a function to conduct the analysis and a function to extract the result of the test
#' @examples
#' # these examples don't work yet
#' # declare_analysis(analysis = "diff-in-means")
#' # declare_analysis(analysis = function(Y, Z, data) lm(paste(Y, "~", Z), data = data))
#' @rdname declare_analysis
#' @export
declare_analysis <- function(formula, treatment_variable = "Z", outcome_variable = NULL, 
                             estimator = difference_in_means, subset = NULL, weights = NULL, ...,
                             quantity_of_interest = NULL, quantity_of_interest_labels = substitute(quantity_of_interest), 
                             estimand_formula = formula, estimand = estimator, 
                             estimand_subset = subset, estimand_weights = weights, 
                             estimand_options = NULL, estimand_quantity_of_interest = quantity_of_interest, 
                             estimand_quantity_of_interest_labels = substitute(estimand_quantity_of_interest)) {
  
  estimator_options <- list(...)
  
  if(is.null(estimand_options) & substitute(estimand) == "estimator")
    estimand_options <- estimator_options
  
  arguments <- mget(names(formals()),sys.frame(sys.nframe()))
  arguments$... <- NULL
  if(length(estimator_options) > 0) {
    for(k in 1:length(estimator_options))
      arguments[[names(estimator_options)[[k]]]] <- estimator_options[[k]]
  }
  
  outcome_variable <- all.vars(formula[[2]])
  
  if(is.null(quantity_of_interest) & substitute(estimator) == "linear_regression" | substitute(estimator) == "probit_regression" | substitute(estimator) == "logistic_regression" | substitute(estimator) == "lm" | substitute(estimator) == "glm" | substitute(estimator) == "vglm")
    stop("If you choose linear, logistic, or probit regression or another standard R modeling function such as glm as the estimator, you must set quantity_of_interest to a function that extracts the QOI from the regression output, such as average_treatment_effect.")
  
  if(is.null(treatment_variable))
    stop("The treatment variable must be declared in the treatment_variable argument.")
  
  treat_coef_num <- which(attr(terms.formula(formula), "term.labels") == treatment_variable) + 
    as.numeric(attr(terms.formula(formula), "intercept") == 1)
  
  if(substitute(estimator) == "difference_in_means" & (length(all.vars(formula)) > 2 | !(treatment_variable %in% all.vars(formula[[3]])) ))
    stop("When using the difference_in_means method, there should only be one covariate listed in the formula on the right-hand side: the treatment variable.")
  
  if(is.null(outcome_variable) & is.null(formula))
    stop("If you do not declare a formula, you must declare the outcome using the outcome_variable field.")
  
  estimate_function <- function(data){
    argument_names <- names(formals(estimator))
    if(!is.null(formula) & "formula" %in% argument_names)
      estimator_options$formula <- stats::formula(unclass(formula))
    if(!is.null(subset) & "subset" %in% argument_names)
      estimator_options$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights) & "weights" %in% argument_names)
      estimator_options$weights <- data[, weights]
    estimator_options$data <- data
    
    return(do.call(estimator, args = estimator_options)) ##, list(formula = stats::formula(unclass(formula)), data = data))))
  }
  
  estimand_function <- function(data){
    argument_names <- names(formals(estimand))
    if(!is.null(estimand_formula) & "formula" %in% argument_names)
      estimand_options$formula <- stats::formula(unclass(estimand_formula))
    if(!is.null(estimand_subset) & "subset" %in% argument_names)
      estimand_options$subset <- with(data, eval(parse(text = estimand_subset)))
    if(!is.null(estimand_weights) & "weights" %in% argument_names)
      estimand_options$weights <- data[, estimand_weights]
    estimand_options$data <- data
    
    return(do.call(estimand, args = estimand_options)) ##, list(formula = stats::formula(unclass(formula)), data = data))))
  }
  
  if(!is.null(quantity_of_interest))
    environment(quantity_of_interest) <- environment()
  if(!is.null(estimand_quantity_of_interest))
    environment(estimand_quantity_of_interest) <- environment()
  
  return_object <- list(estimate = estimate_function, estimand = estimand_function, 
                        quantity_of_interest = quantity_of_interest, 
                        estimand_quantity_of_interest = estimand_quantity_of_interest,
                        treatment_variable = treatment_variable,
                        outcome_variable = outcome_variable, arguments = arguments,
                        call = match.call())
  if(is.null(quantity_of_interest))
    return_object$quantity_of_interest <- NULL
  if(is.null(estimand_quantity_of_interest))
    return_object$estimand_quantity_of_interest <- NULL  
  
  structure(return_object, class = "analysis")
  
}

#' @export
linear_regression <- lm

#' @export
logistic_regression <- function(formula, subset = NULL, weights = NULL, data, ...){
  args_list <- c(list(formula = formula, subset = subset, weights = weights, data = data), list(...))
  args_list$family <- binomial(link = "logit")
  do.call(glm, args = args_list)
}

#' @export
probit_regression <- function(formula, subset = NULL, weights = NULL, data, ...){
  args_list <- c(list(formula = formula, subset = subset, weights = weights, data = data), list(...))
  args_list$family <- binomial(link = "probit")
  do.call(glm, args = args_list)
}

#' @export
average_treatment_effect <- function(x, statistics = c("est", "se", "p", "ci_lower", "ci_upper", "df")){
  coef_name <- names(coef(x))[treat_coef_num]
  df <- df.residual(x)
  est <- coef(x)[treat_coef_num]
  se <- sqrt(diag(vcov(x)))[treat_coef_num]
  p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
  conf_int <- suppressMessages(confint(x))[treat_coef_num, ]
  
  output <- matrix(c(est, se, p, conf_int, df), 
                   dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                   paste(outcome_variable, "~", coef_name, "_", 
                                         quantity_of_interest_labels, sep = "")))
  
  return(output[which(rownames(output) %in% statistics), , drop = FALSE])
}

#' @export
difference_in_means <- function(formula, data, weights = NULL, subset = NULL, alpha = .05) {
  
  if(length(all.vars(formula[[3]]))>1)
    stop("The formula should only include one variable on the right-hand side: the treatment variable.")
  
  d_i_m <- function(Y, t, w, cond1, cond2, alpha){
    N <- length(Y)
    diff <- mean(Y[t == cond1]) - mean(Y[t == cond2])
    se <- sqrt(var(Y[t == cond1])/sum(t == cond1) + var(Y[t == cond2])/sum(t == cond2))
    df <- length(Y) - 2
    p <- 2 * pt(abs(diff/se), df = N - 2, lower.tail = FALSE)
    ci_lower <- diff - qt(1 - alpha/2, df = N - 2) * se
    ci_upper <- diff + qt(1 - alpha/2, df = N - 2) * se
    return(c(diff, se, p, ci_lower, ci_upper, df))
  }
  
  condition_names <- unique(data[,all.vars(formula[[3]])])
  combn <- combn(rev(sort(condition_names)), m = 2)
  combn_names <- apply(combn, 2, function(x) paste(x, collapse = "-"))
  
  if(!is.null(subset))
    data <- data[subset, ]
  Y <- data[, all.vars(formula[[2]])]
  t <- data[, all.vars(formula[[3]])]
  if(!is.null(weights))
    w <- weights[subset]
  
  return_matrix <- matrix(NA, nrow = 6, ncol = ncol(combn), 
                          dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                          paste0(all.vars(formula[[2]]), "~", combn_names, 
                                                 "_diff_in_means")))
  for(c in 1:ncol(combn)){
    return_matrix[, c] <- d_i_m(Y = Y, t = t, w = w, cond1 = combn[1, c], cond2 = combn[2, c], alpha = alpha)
  }
  
  return(return_matrix)
}

#' @export
difference_in_means_blocked <- function(formula, data, block_variable = NULL, subset = NULL, alpha = .05) {
  
  if(is.null(block_variable))
    stop("This difference-in-means estimator can only be used if you specify block_variable, a string indicating which variable in the data frame contains the blocks.")
  
  if(length(all.vars(formula[[3]]))>1)
    stop("The formula should only include one variable on the right-hand side: the treatment variable.")
  
  d_i_m_blocked <- function(Y, t, b, cond1, cond2, alpha){
    
    N <- length(Y)
    
    block_names <- sort(unique(b))
    
    block_weights <- (sapply(block_names, function(i) sum(b==i)))/N
    
    f <- function(x){
      t = tapply(Y,list(t, b), mean,na.rm=TRUE)
      (block_weights %*% (t[2,]-t[1,]))[1,1]
    }
    
    diff <- f(X) 
    vars <- sapply(block_names, function(i)  {
      var(Y[b==i & t == cond1], na.rm = TRUE )/sum(b==i & t == cond1)+
        var(Y[b==i & t == cond2], na.rm = TRUE )/sum(b==i & t == cond2)})
    se  <- (block_weights^2 %*% vars)^.5

    df <- length(Y) - 2
    p <- 2 * pt(abs(diff/se), df = N - 2, lower.tail = FALSE)
    ci_lower <- diff - qt(1 - alpha/2, df = N - 2) * se
    ci_upper <- diff + qt(1 - alpha/2, df = N - 2) * se
    return(c(diff, se, p, ci_lower, ci_upper, df))
  }
  
  condition_names <- unique(data[,all.vars(formula[[3]])])
  combn <- combn(rev(sort(condition_names)), m = 2)
  combn_names <- apply(combn, 2, function(x) paste(x, collapse = "-"))
  
  if(!is.null(subset))
    data <- data[subset, ]
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


#' @param formula  what is it?
#' @param treatment_variable  what is it?
#' @param subset  what is it?
#' @param data  what is it?
#' @param sep  what is it?
#' @rdname declare_analysis
#' @export
truth_data_frame <- function(formula = NULL, treatment_variable = "Z", 
                             weights = NULL, subset = NULL, data = data, sep = "_", ...) {
  
  if(is.null(formula))
    stop("Formula must be provided.")
  
  outcome_variable <- all.vars(formula[[2]])
  
  other_variable_names <- all.vars(formula[[3]])[!(all.vars(formula[[3]]) %in% treatment_variable)]
  
  if(!missing(cluster_variable))
    other_variable_names <- c(other_variable_names, cluster_variable)
  
  if(!missing(block_variable))
    other_variable_names <- c(other_variable_names, block_variable)
  
  treatment_conditions <- unique(data[, treatment_variable])
  
  potential_outcome_variable_names <- paste(outcome_variable, sep, treatment_conditions, sep = "")
  
  ## create replicated data frame with only the right variables
  data <- data[, c(potential_outcome_variable_names, other_variable_names)]
  data_rep <- do.call("rbind", replicate(length(treatment_conditions), data, simplify = FALSE))
  
  ## replace treatment with the replicated treatment
  data_rep[, treatment_variable] <- rep(treatment_conditions, each = nrow(data))
  
  data_rep[, outcome_variable] <- observed_outcome(outcome = outcome_variable, 
                                                   treatment_assignment = treatment_variable,
                                                   data = data_rep, sep = sep)
  
  if(!is.null(subset))
    data <- subset(data, eval(parse(text = subset)))
  
  return(data_rep)
  
}

#' @param analysis what is it?
#' @param data what is it?
#' @rdname declare_analysis
#' @export
get_estimates_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  if(is.null(analysis$estimate))
    stop("This analysis function does not have a model associated with it. Try get_estimates to obtain the quantities of interest.")
  return(analysis$estimate(data = data))
}

#' @rdname declare_analysis
#' @export
get_estimands_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  if(is.null(analysis$estimand))
    stop("This analysis function does not have a model associated with it. Try get_estimands to obtain the quantities of interest for the estimand.")
  return(analysis$estimand(data = truth_data_frame(formula = analysis$arguments$estimand_formula, data = data)))
}

#' @param analysis what is it?
#' @param qoi what is it?
#' @param data what is it?
#' @rdname declare_analysis
#' @export
get_estimates <- function(analysis, quantity_of_interest = NULL, data, analysis_labels = NULL) {
  
  ## extract names of arguments analysis objects
  if(is.null(analysis_labels)){
    if(class(analysis) == "list")
      analysis_labels <- paste(substitute(analysis)[-1L])
    else
      analysis_labels <- paste(substitute(analysis))
  }
  
  if(!is.null(quantity_of_interest)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(quantity_of_interest(analysis, data = data))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
      ## if the user sends no qoi function but does send a list of analysis objects,
      ## run this function on each analysis object and cbind the results
      estimates_list <- list()
      for(i in 1:length(analysis)) {
        if(!is.null(analysis[[i]]$quantity_of_interest)){
          estimates_list[[i]] <- analysis[[i]]$quantity_of_interest(get_estimates_model(analysis = analysis[[i]], data = data))
        } else {
          estimates_list[[i]] <- analysis[[i]]$estimate(data = data)
        }
        if(class(estimates_list[[i]]) != "matrix" & class(estimates_list[[i]]) != "data.frame")
          stop(paste("The quantity_of_interest function you set, or in its absence the estimate function, for analysis named", analysis_labels[i], "did not produce a matrix or data frame of results."))
        colnames(estimates_list[[i]]) <- paste(colnames(estimates_list[[i]]), analysis_labels[i], sep = "_")
      }
      
      ## this merges the summary statistics together such that there can be different statistics for each analysis
      ## and they are merged and named correctly
      estimates_matrix <- estimates_list[[1]]
      if(length(analysis) > 1){
        for(i in 2:length(analysis)){
          estimates_matrix <- merge(estimates_matrix, estimates_list[[i]], by = "row.names", all.x = T, all.y = T)
          rownames(estimates_matrix) <- estimates_matrix[,1]
          estimates_matrix <- estimates_matrix[, 2:ncol(estimates_matrix), drop = F]
        }
      }
      return(estimates_matrix)
    } else {
      if(class(analysis) != "analysis")
        stop("The object in the analysis argument must by created by the declare_analysis function.")
      ## otherwise process the one analysis function
      if(!is.null(analysis$quantity_of_interest)){
        estimates_matrix <- analysis$quantity_of_interest(get_estimates_model(analysis = analysis, data = data))
      } else {
        estimates_matrix <- analysis$estimate(data = data)
      }
      if(class(estimates_matrix) != "matrix" & class(estimates_matrix) != "data.frame")
        stop(paste("The quantity_of_interest function you set, or in its absence the estimate function, for analysis named", analysis_labels, "did not produce a matrix or data frame of results."))
      colnames(estimates_matrix) <- paste(colnames(estimates_matrix), analysis_labels[1], sep = "_")
      return(estimates_matrix)
    }
  }
}

#' @param analysis what is it?
#' @param qoi  what is it?
#' @param data  what is it?
#' @param statistics  what is it?
#' @rdname declare_analysis
#' @export
get_estimands <- function(analysis, qoi = NULL, data, statistics = "est", analysis_labels = NULL){
  
  if(is.null(analysis_labels)){
    if(class(analysis) == "list")
      analysis_labels <- paste(substitute(analysis)[-1L])
    else
      analysis_labels <- paste(substitute(analysis))
  }
  
  if(!is.null(qoi)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(qoi(analysis, data = truth_data_frame(formula = analysis$arguments$estimand_formula, data = data)))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
      ## if the user sends no qoi function but does send a list of analysis objects,
      ## run this function on each analysis object and cbind the results
      estimands_list <- list()
      for(i in 1:length(analysis)){
        if(!is.null(analysis[[i]]$estimand_quantity_of_interest)){
          estimands_list[[i]] <- analysis[[i]]$estimand_quantity_of_interest(get_estimands_model(analysis = analysis[[i]], data = data), statistics = statistics)
          ## get_estimands_model does truth_data_frame, so just sending it data
        } else {
          estimands_list[[i]] <- analysis[[i]]$estimand(truth_data_frame(formula = analysis[[i]]$arguments$estimand_formula, data = data))
        }
        colnames(estimands_list[[i]]) <- paste(colnames(estimands_list[[i]]), analysis_labels[i], sep = "_")
      }
      ## when it is sent back to get_estimands() it will run truth_data_frame, so just sending it data
      
      ## this merges the summary statistics together such that there can be different statistics for each analysis
      ## and they are merged and named correctly
      estimands_matrix <- estimands_list[[1]]
      if(length(analysis) > 1){
        for(i in 2:length(analysis)){
          estimands_matrix <- merge(estimands_matrix, estimands_list[[i]], by = "row.names", all.x = T, all.y = T)
          rownames(estimands_matrix) <- estimands_matrix[,1]
          estimands_matrix <- estimands_matrix[, 2:ncol(estimands_matrix), drop = F]
        }
      }
      return(estimands_matrix)
    } else {
      if(class(analysis) != "analysis")
        stop("The object in the analysis argument must by created by the declare_analysis function.")
      ## otherwise process the one analysis function
      if(!is.null(analysis$estimand_quantity_of_interest)){
        estimands_matrix <- analysis$estimand_quantity_of_interest(get_estimands_model(analysis = analysis, data = data), statistics = statistics)
        ## get_estimands_model does truth_data_frame, so just sending it data
      } else {
        estimands_matrix <- analysis$estimand(truth_data_frame(formula = analysis$arguments$estimand_formula, data = data))
      }
      colnames(estimands_matrix) <- paste(colnames(estimands_matrix), analysis_labels[1], sep = "_")
      return(estimands_matrix)
    }
  }
}

#' @export
analysis_outcome_variable <- function(analysis) {
  return(analysis$outcome_variable)
}

#' @export
analysis_treatment_variable <- function(analysis) {
  return(analysis$treatment_variable)
}






