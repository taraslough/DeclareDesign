



formula <- Y ~ .01 + 0*Z0 + .15*Z1 + .1*I(income^2) - .15*Z1*income

parse_outcome_equation <- function(formula){
  
  outcome_name <- all.vars(formula[[2]])
  
  formula_rhs <- as.character(formula)[[3]]
  
  
  
  ##formula_rhs_expressions <- strsplit(formula_rhs, c("+", "-"), fixed = TRUE)[[1]]
  
  
  
  
  
  treat_coef_num <- which(attr(terms.formula(formula), "term.labels") == treatment_variable) + 
    as.numeric(attr(terms.formula(formula), "intercept") == 1)
  
  

  
  return(list(outcome_name = outcome_name, R_formula = y, true_coefficients = z))
  
}
