#' Declare the structure of the sample frame 
#'
#' @param ... a list either of variable declarations, or of lists of variable declarations (one per level)
#' @param N_per_level description
#' @param lower_units_per_level description
#' @export
declare_sample_frame <-
  function(...,N_per_level=NULL,lower_units_per_level=NULL) {
    
    # Put in checks here to check how the list is structured
    # i.e. if there are variable declarations or functions in the top level it should
    # only be those, no lists
    # and if there are levels, then the levels should be named and have
    
    variable_list  <- list(...)
    
    if(TRUE %in% (c("function","DGP_object")%in%sapply(variable_list,class))){
      N_levels <- 1
      make_sample <- function(){make_X_matrix(variables = variable_list,
                                              variable_names = names(variable_list),
                                              N = N_units
      )} 
      level_names <- "Only one level"
      # data_structure_description <- "Only one level"
      variable_names <- names(variable_list)
      
    }else{
      
      N_levels       <- length(variable_list)
      level_names    <- names(variable_list)
      variable_names <- lapply(variable_list, names)
      
      # Make object here stating number of levels, which 
      # looks at each variable_list and tries to figure it out:
      if(is.null(N_per_level)){
        # N_per_level    <- lapply(1:N_levels,level_names)
      }
      
      if(is.null(lower_units_per_level)){
        # lower_units_per_level    <- Some division
      }
      
      make_sample <- function(){
        X_list <- lapply(1:N_levels,function(i){
          X_mat <- make_X_matrix(variables = variable_list[[i]],
                                 variable_names = variable_names[[i]],
                                 N = N_per_level[i])
          X_mat$id <- 1:dim(X_mat)[1]
          names(X_mat)[names(X_mat)=="id"] <- paste0(level_names[i],"_id")
          return(X_mat)
        })
        
        id_vars <- sapply(X_list,function(X_to_grab){
          X_names <- names(X_to_grab)
          return(X_names[length(X_names)])
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
        
        return(sample_matrix)
      }
      
      
      #     data_structure_description <- paste0(paste(sapply(2:N_levels,function(i){
      #       if(nested_levels[i-1]){
      #         paste0(level_names[i-1], " is / are nested within ",level_names[i])
      #       }else{paste0(level_names[i-1], 
      #                    " is / are not nested within ",
      #                    level_names[i])}
      #       }),collapse = ", "),".")
      
    }
    sample_frame_object <- list(
      make_sample = make_sample,
      covariate_names = variable_names,
      level_names = level_names,
      number_levels = N_levels,
      N_per_level = N_per_level,
      lower_units_per_level = lower_units_per_level,
      # data_structure_description = data_structure_description,
      call = match.call()
    )
    class(sample_frame_object) <- "sample_frame"
    return(sample_frame_object)
  }


#' @export 
make_X_matrix <-
  function(...,variable_names = NULL,N){
    
    variables  <- list(...)
    
    class_list <- sapply(variables,class)
    if(all(c("function","list")%in%class_list)|
       all(c("DGP_object","list")%in%class_list))stop(
         "Function takes either direct variable declarations or a list of variable declarations, not both at once.")
    if("list"%in%class_list){
      variables <- unlist(variables,recursive = F)
    }
    
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
        return(function(){rnorm(n=N,mean = variable$mean, sd = variable$sd)})
      }
      if (variable$distribution == "binary") {
        return(function(){binom_out <- rbinom(n = N,size = 1,prob =variable$probability)
        if(!is.null(variable$categories)){binom_out <- 
          factor(binom_out,c(0,1),
                 variable$categories)}
        return(binom_out)})
      }
      if (variable$distribution == "multinomial") {
        return(function(){multinom_out <- 
          apply(rmultinom(n = N,size = 1,
                          prob =variable$probability),2,function(i)which(i==1))
        if(!is.null(variable$categories)){
          multinom_out <- factor(multinom_out,
                                 levels = 1:length(variable$categories),
                                 labels = variable$categories)}
        return(multinom_out)})
      }
    })
    x <- lapply(fun.list,function(f)f())
    X.char <- do.call(cbind.data.frame,x)
    X <- data.frame(matrix(rep(NA,dim(X.char)[1]*dim(X.char)[2]),nrow = dim(X.char)[1]))
    for(i in 1:dim(X.char)[2]){
      X[,i] <- X.char[i][,1]
    }
    if(!is.null(variable_names))names(X) <- variable_names
    if(is.null(variable_names))names(X) <- names_list
    return(X)
  }


