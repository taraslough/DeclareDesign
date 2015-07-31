#' Declare the structure of the sample frame 
#'
#' @param ... a list either of variable declarations, or of lists of variable declarations (one per level)
#' @param N_per_level vector of the sample sizes per level, one number per level
#' @param lower_units_per_level description
#' @param N total sample size of the sample frame
#' @param data optional data frame to include variables that are not defined in the sample frame
#' @param resample when data are provided, indicates whether the data is resampled. By default, the data is returned as is. Resampling will automatically respect the levels defined by the variable declarations.
#' @param level_ID_variables optional strings indicating the variable names for the identifiers of each level, i.e. c("individual_id", "village_id")
#' @export
declare_sample_frame <- function(..., N_per_level = NULL, lower_units_per_level = NULL, N = NULL, data = NULL, resample = FALSE, level_ID_variables = NULL) {
  
  
  if(!is.null(N_per_level) & !is.null(N)) {
    stop("You may not specify N and N_per_level simultaneously.")
  }
  
  if(!is.null(lower_units_per_level) & !is.null(N)){
    stop("You may not specify N and lower_units_per_level simultaneously.")
  }
  
  if(!is.null(lower_units_per_level) & !is.null(N_per_level)){
    stop("You may not specify N_per_level and lower_units_per_level simultaneously.") 
  }
  
  if(is.null(N_per_level) & is.null(N) & is.null(lower_units_per_level) & 
     (is.null(data) | (!is.null(data) & resample == TRUE))){
    stop("You must either specify N, lower_units_per_level, N_per_level.")
  }
  
  # If they provide N, define N_per_level
  if(is.null(N_per_level) & !is.null(N)){
    N_per_level <- N
  } 
  
  if(!all(diff(N_per_level)<0))
    stop("Each level in N_per_level should be smaller than the preceding level.")
  
  # Logical test if a single list of variable declarations was supplied to ...
#   list_test <- function(...){
#     arguments <- list(...)
#     all(sapply(arguments,class)=="list")&length(arguments)==1
#   }
  
  variable_list  <- list(...)
  
#   if(list_test(variable_list)){
#     variable_list <- variable_list[[1]]
#   }
  
  # If there are no variables specified, just data structure
  if(length(variable_list)==0){
    # If N_per_level was multi-level and lower_units_per_level was not supplied, 
    # make lower_units_per_level
    if(length(N_per_level) > 1 & is.null(lower_units_per_level)){
      
      lower_units_per_level <- 
        list(rep(NA,
                 length(N_per_level)))
      lower_units_per_level[2:length(N_per_level)] <- 
        lapply(2:length(N_per_level),
               function(i){
                 remaindr(N_per_level[i-1],N_per_level[i])
               })
      lower_units_per_level[[1]] <- 
        rep(1,N_per_level[1])
    }
  }
  # If only lower_units_per_level is supplied
  if(!is.null(lower_units_per_level)&is.null(N_per_level)){
    N_per_level <- sapply(lower_units_per_level,length)
    
    # Test that the lower_units structure is correct
    lower_units_test <- sapply(length(lower_units_per_level):2,
                               function(i){
                                 sum(lower_units_per_level[[i]])==
                                   length(lower_units_per_level[[i-1]])})
    
    if(!all(lower_units_test)){
      stop("The argument supplied to lower_units_per_level is not logical. The sum of every higher level should be equal to the length of the preceding lower level. For example, in a study with 4 units and 2 groups, lower_units_per_level = list(c(1,1,1,1),c(2,2)).")
    }
  }
  # Don't know why this was here: if N_per_level and N were not supplied, 
  # then lower_units_per_level must have been supplied
#   if(!is.null(N_per_level)){
#     lower_units_per_level <- 
#       list(rep(NA, length(N_per_level)))
#     
#     if(length(N_per_level)>1){
#       lower_units_per_level[2:length(N_per_level)] <- 
#         lapply(2:length(N_per_level),
#                function(i){
#                  remaindr(N_per_level[i-1],N_per_level[i])
#                })        
#     }
#     lower_units_per_level[[1]] <- 
#       rep(1,N_per_level[1])   
#   }
  
  
  if(!length(variable_list)==0){
    
    level_names <- names(variable_list)
  } else {
    level_names <- paste0("level_", 1:length(N_per_level))
  }
  
  id_vars <- paste0(level_names,"_id")
  
  if(TRUE %in% (c("function","DGP_object") %in% sapply(variable_list,class))){
    N_levels <- 1
    if(is.null(data)){
      make_sample <- function(){make_X_matrix(variable_list,
                                              N = N
      )} 
    }
    level_names <- "level_1"
    variable_names <- names(variable_list)
    
  } else {
    
    N_levels       <- length(N_per_level)
    
    variable_names <- lapply(variable_list, names)
    
    if(is.null(data)){
      make_sample <- function(){
        
        X_list <- lapply(1:N_levels,function(i){
          # When there's variables
          
          if(length(variable_list) >0){
            X_mat <- make_X_matrix(variables = variable_list[[i]],
                                   # variable_names = variable_names[[i]],
                                   N = N_per_level[i])
            X_mat$id <- 1:dim(X_mat)[1]
            names(X_mat)[names(X_mat)=="id"] <- paste0(level_names[i],"_id")
          }else{
            X_mat <- matrix(1:N_per_level[i], 
                            dimnames = list(NULL, paste0(level_names[i],"_id")))
          }
          return(data.frame(X_mat))
        })
        
        sample_matrix <- X_list[[1]]
        
        if(N_levels > 1){ 
          X_mat_joined <- NA
          for(i in N_levels:2){
            X_list[[i-1]]$merge_id <- 
              sample(rep(X_list[[i]][,id_vars[i]],
                         lower_units_per_level[[i]]))
            
            names(X_list[[i-1]])[names(X_list[[i-1]])=="merge_id"] <- id_vars[i]
            
            X_mat_joined <- 
              merge(x  = X_list[[i-1]],
                    y  = X_list[[i]],
                    by = id_vars[i])
          }
          sample_matrix <- X_mat_joined
        }
        
        sample_matrix <- sample_matrix[order(sample_matrix[,id_vars[1]]), , drop=FALSE]
        return(sample_matrix)
      }
    }
    
  }
  
  if(!is.null(data)){
    
    if(resample == TRUE){
      
      user_data <- data
      ## if data is sent into make_sample, this does not work (data is NULL), must have different name
      
      if(N_levels == 1) {
        make_sample <- function(){
          return(user_data[sample(1:nrow(user_data), N, replace = TRUE), , drop = FALSE])
        }
      } else if (N_levels > 1){
        make_sample <- function(){
          sample_by_level <- list()
          for(j in N_levels:1){
            if(j == N_levels){
              sample_by_level[[j]] <- sample(user_data[, id_vars[j]], N_per_level[j], replace = TRUE)
            } else {
              ## now go through each of the units in the level above it
              sample_current_level <- c()
              for(k in sample_by_level[[j+1]]){
                sample_current_level <- c(sample_current_level, 
                                          sample(user_data[user_data[, id_vars[j+1]] == k, id_vars[j]], 
                                                 round(N_per_level[j]/N_per_level[j+1]), replace = TRUE))
              }
              sample_by_level[[j]] <- sample_current_level
            }
          }
          return(user_data[sample_by_level[[1]], , drop = FALSE])
        }
      }
      
    } else {
      
      if(!is.null(N) | !is.null(N_per_level) | !is.null(lower_units_per_level))
        stop("Please do not provide N, N_per_level, or lower_units_per_level when resample is set to FALSE and you provided a dataframe.")
      make_sample <- NULL
      
    }
    
  }
  
  sample_frame_object <- list(
    make_sample = make_sample,
    data = data,
    covariate_names = variable_names,
    level_names = level_names,
    number_levels = N_levels,
    N_per_level = N_per_level,
    lower_units_per_level = lower_units_per_level,
    call = match.call()
  )
  
  class(sample_frame_object) <- "sample_frame"
  return(sample_frame_object)
}


#' @export 
make_X_matrix <- function(...,variable_names = NULL,N) {
  variables  <- list(...)
  
  
  class_list <- sapply(variables,class)
  
  if (all(c("function","list") %in% class_list) |
      all(c("DGP_object","list") %in% class_list))
    stop(
      "Function takes either direct variable declarations or a list of variable declarations, not both at once."
    )
  
  if ("list" %in% class_list) {
    variables <- unlist(variables,recursive = F)
  }
  
  
  transformation_check <- sapply(variables,function(variable_elements){
    element_names <- names(variable_elements)
    is_transformation <- "transformation"%in%element_names
    return(is_transformation)
  })
  
  transformations <- variables[which(transformation_check)]
  variables <- variables[which(!transformation_check)]
  
  names_list <- names(variables)
  
  
  fun.list <- lapply(variables,function(variable) {
    if (!class(variable) %in% c("function","DGP_object")) {
      stop(
        "Variables should either be random number functions or DGP_object (see declare_variable())"
      )
    }
    if (class(variable) == "function") {
      return(variable)
    }
    if (variable$distribution == "normal") {
      return(function() {
        rnorm(n = N,mean = variable$mean, sd = variable$sd)
      })
    }
    if (variable$distribution == "binary") {
      return(function() {
        binom_out <- rbinom(n = N,size = 1,prob = variable$probability)
        if (!is.null(variable$categories)) {
          binom_out <-
            factor(binom_out,c(0,1),
                   variable$categories)
        }
        return(binom_out)
      })
    }
    if (variable$distribution == "multinomial") {
      return(function() {
        multinom_out <-
          apply(rmultinom(
            n = N,size = 1,
            prob = variable$probability
          ),2,function(i)
            which(i == 1))
        if (!is.null(variable$categories)) {
          multinom_out <- factor(
            multinom_out,
            levels = 1:length(variable$categories),
            labels = variable$categories
          )
        }
        return(multinom_out)
      })
    }
  })
  
  x <- lapply(fun.list,function(each_function){each_function()})
  
  X.char <- do.call(cbind.data.frame,x)
  
  X <- data.frame(matrix(rep(NA, dim(X.char)[1] * dim(X.char)[2]),
                         nrow = dim(X.char)[1]))
  
  for (i in 1:dim(X.char)[2]) {
    X[,i] <- X.char[i][,1]
  }
  
  if (!is.null(variable_names)) {
    names(X) <- variable_names
  }
  
  if (is.null(variable_names)) {
    names(X) <- names_list
  }
  
  if(length(transformations)>0){
    
    transformation_names <- names(transformations)
    transformation_calls <- sapply(transformations,function(trans){
      trans$transformation
    })
    
    for(i in 1:length(transformation_names)){
      X[transformation_names[i]] <- with(data = X,expr = eval(parse(text = transformation_calls[i])))
    }
  }
  return(X)
}

#' @export
remaindr <- function(numerator,denominator) {
  m_each <- rep(numerator %/% denominator, denominator)
  remainder <- numerator %% denominator
  m_each <-
    m_each + ifelse(1:denominator %in% sample(1:denominator, remainder), 1, 0)
  return(m_each)
}

#' @rdname declare_sample_frame
#' @export
summary.sample_frame <- function(x){
  
  summ <- data.frame(x$level_names, x$N_per_level, "")
  colnames(summ) <- c("Level", "Units per level", "Description")
  summ
  
}

#' Print a table describing covariates described in sample_frame
#'
#' @param sample_frame a sample_frame object created with declare_sample frame.
#' @export
covariates_table <- function(sample_frame){
  cat("This will be a summary table of the distribution of each covariate at each level. Not implemented yet.")
}






