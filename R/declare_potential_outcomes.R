#' Define the potential outcomes of the experiment
#'
#' @param condition_names A character vector indicating the names of the conditions to which subjects can be assigned. Conceptually identical to the potential outcomes that are revealed in each condition, implicitly invoking SUTVA.
#' @param outcome_formula A regression-like expression for declaring the relationship between the outcome, treatments, and optionally, the covariates.
#' @param outcome_variable_DGP using \code{\link{declare_variable}}, a function for describing the data-generating process of the outcome variable.
#' @param cluster_variable The name of the clustering variable
#' @param ICC the intracluster correlation coefficient.  Note that if the outcome formula includes covariates, and those covariates are correlated with clusters, the true ICC may be higher or lower than is declared by this argument.
#' @param population_proportions what is it?
#' @return outcomes_object
#' @export
declare_potential_outcomes <- function(condition_names = NULL,outcome_formula = NULL,outcome_variable_DGP = declare_variable(),cluster_variable = NULL,ICC = NULL,population_proportions = NULL,proportion_outcome_name = NULL){
  

  
  if(!is.null(population_proportions)){
    
    warning("You have defined the potential outcomes in terms of population-level changes in proportions. Consequently, potential outcomes cannot be a function covariates. The following arguments will be ignored: \n outcome_formula \n outcome_variable_DGP \n cluster_variable \n ICC")
    
    if(any(class(population_proportions)%in%c("numeric","double"))){
      
      warning("Assuming that population_proportions[i] = Pr(outcome[i]==1).")
      
      if(any(population_proportions>1|population_proportions<0)){
        stop("Population proportions must be in the interval [0,1].")
      }
      
      failure_prob <- rep(1,length(population_proportions))-population_proportions
      
      population_proportions <- matrix(
        data = c(failure_prob,
                 population_proportions),
        nrow = 2,
        ncol = length(population_proportions),
        byrow = TRUE,
        dimnames = list(c(0,1),
                        c(condition_names))
      )
    }
    
    
    if(!any(class(population_proportions)%in%c("data.frame","matrix"))){
      stop("You must supply either a vector of proportions or a matrix of proportions with condition names in the columns and outcome names in the rows.")}
    
    if(!all(apply(population_proportions,2,sum,na.rm = T)==1)){
      stop("All columns of the population_proportions matrix should sum to 1. Check that you have conditions as the column names and outcomes as the row names.")
    }
    
    if(is.null(colnames(population_proportions))&is.null(condition_names)){
      stop("You must supply at least the condition_names or give condition names in the colnames of population_proportions.")
    }
    
    if(is.null(colnames(population_proportions))&!is.null(condition_names)){
      colnames(population_proportions) <- condition_names
    }else{
      if(is.null(condition_names)&!is.null(colnames(population_proportions))){
        condition_names <- colnames(population_proportions)
      }else{
        if(!all(condition_names==colnames(population_proportions))){
          stop("The order and spelling of condition_names and the column names in population_proportions should be identical.")
        }
      }
    }
    
    if(is.null(rownames(population_proportions))){
      if(dim(population_proportions)[1]==2){
        warning("No rownames supplied in population_proportions, assuming row 1 = 0 and row 2 = 1.")
        rownames(population_proportions) <- c(0,1)
      }
      else{stop("You must specify the names of the outcomes in the rows of population_proportions.")}
    }
    
    if(is.null(proportion_outcome_name)){
      warning("Outcome name not supplied, defaulting to 'Y'.")
      proportion_outcome_name <- "Y"
    }
    
    outcomes_object <- list(
      condition_names  = condition_names,
      population_proportions = population_proportions,
      proportion_outcome_name = proportion_outcome_name,
      call = match.call()
    )
    
    class(outcomes_object) <- "potential_outcomes"
    
    return(outcomes_object)
    
  }else{
    
    
    if(is.null(condition_names)){
      stop("Please use condition_names to specify the names of the conditions to which units are assigned.")
    }
    
    if(!all(condition_names %in% all.vars(outcome_formula)))stop(
      "All conditions must be included as variables in the outcome formula."
    )
    
    if(is.null(outcome_formula)){
      stop("Please specify the data generating process as a formula that generates the outcomes as a function of the treatment and/or covariates, for example: \n Y ~ 1 + 0*control + .5*treatment + .2*income")
    }
    
    outcomes_object <- list(
      condition_names  = condition_names,
      outcome_formula  = outcome_formula,
      outcome_name     = all.vars(outcome_formula)[1],
      outcome_variable = outcome_variable_DGP,
      cluster_variable = cluster_variable,
      ICC = .01,
      call = match.call()
    )
    class(outcomes_object) <- "potential_outcomes"
    return(outcomes_object)}
}


#' @export
outcomes_table <- function(x){
  if(class(x) == "potential_outcomes")
    x <- list(x)
  cat("This will be a summary table of the distribution of each outcome. Not implemented yet.")
}



