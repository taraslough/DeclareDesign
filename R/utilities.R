integerize <- function(data){
  for(i in 1:ncol(data)){
    numeric_check <- FALSE
    numeric_check <- class(data[,i]) %in% c("numeric","integer")
    
    if(!numeric_check){
      suppressWarnings(numeric_check <- identical(data[,i], as.factor(as.integer(as.character(data[,i])))))
      if(!numeric_check){
        suppressWarnings(numeric_check <- identical(data[,i], as.factor(as.numeric(as.character(data[,i])))))
        if(!numeric_check){
          suppressWarnings(numeric_check <- identical(data[,i], as.numeric(as.character(data[,i]))))
          if(!numeric_check){
            suppressWarnings(numeric_check <- identical(data[,i], as.numeric(as.character(data[,i]))))
          }
        }
      }
      if(numeric_check){
        data[,i] <- as.integer(as.character(data[,i]))
      }
    }
  }
  return(data)
}

trim_spaces <- function(text){
  gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", text))
}

#' Print version of R and packages to improve reproducibility
#'
#' @export
print_versions <- function(versions = NULL){
  
  if(is.null(versions)){
    versions <- get_versions()
  }
  
  cat("This document was compiled with the R package DeclareDesign version ", versions$DeclareDesign, 
      " using ", versions$R, " with the ", versions$OS, " operating system and the R packages: ", sep = "")
  
  versions$packages[, 2] <- paste0("(", versions$packages[, 2], ")")
  versions$packages <- apply(versions$packages, 1, function(x) paste(x, collapse = " "))
  
  if(length(versions$packages) == 1)
    cat(versions$packages)
  if(length(versions$packages) == 2)
    cat(paste(versions$packages, collapse = " and "))
  else
    cat(paste(versions$packages[1:(length(versions$packages)-1)], collapse = "; "), "; and ", versions$packages[length(versions$packages)], sep = "")
  
  cat(".")
  
}

reorient <- function(x) {
  obj <- c(x)
  names(obj) <- rep(paste(rownames(x), colnames(x), sep = "_"), each = ncol(x))
  return(obj)
}


clean_inputs <- function(object, object_class, accepts_list = TRUE){
  
  if(is.null(object)){
    return(object)
  } else {
  
    if(accepts_list == TRUE){
      
      if(class(object) %in% object_class){
        object <- list(object)
      }
      
      if(class(object) != "list"){
        stop(paste0("The object in the ", object_class, " argument must be created by ", paste0("declare_", object_class, collapse = " or "), " or be a list of objects created by those function(s)."))
      }
      
      if(!all(sapply(object, function(i) class(i) %in% object_class))){ 
        stop(paste0("All objects in the list in the ", object_class, " argument must be created by ", paste0("declare_", object_class, collapse = " or "), "."))
      }
    } else {
      
      ## accepts_list == FALSE
      
      if (!(class(object) %in% object_class)){
        stop(paste0("The object in the ", object_class[1], " argument must be created by ", paste0("declare_", object_class, collapse = " or "), ". It cannot be a list."))
      }
      
    }
    
    return(object)
  }
}

round_condition_names <- function(condition_names, digits = 15){
  if(class(condition_names) != "list"){
    round(condition_names, digits = digits)
  } else {
    lapply(condition_names, function(x) round(x, digits = digits))
  }
}
