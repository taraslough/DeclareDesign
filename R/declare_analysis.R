

get_test_statistic <- function(analysis, data, design){
  
  return(analysis$test_statistic(analysis$analysis(design, data)))
  
}

do_analysis <- function(analysis, data, design){
  
  return(analysis$analysis(design, data))
  
}

declare_analysis <- function(
                    method, ## either string "diff-in-means", "lm-with-covariates" or a function object that takes as arguments data, design and 
                           ## spits out in the simplest version a single treatment effect estimates (for a multi-arm study spits out something more complex)
                    test_statistic ## a function that extracts the test statistic from the analysis
                    ){
  
  if(!class(test_statistic)=="function")
    stop("Currently only functions can be used as test_statistics.")
  
  if(class(method) == "character") {
    if(method == "diff-in-means") {
      method <- function(data, design){
        
        analysis <- function(design, data) lm(paste(outcome_var_name(design), "~", treat_var_name(design)), data = data)
        
        test_statistic <- function(analysis) sqrt(diag(vcov(analysis)))[1, 1]
        
        return(list(analysis = analysis, test_statistic = test_statistic))
      }
    } 
  } else if(class(method) == "function"){
        
    return(list(analysis = method, test_statistic = test_statistic))
    
  }
    
  
}
