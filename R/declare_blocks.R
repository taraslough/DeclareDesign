

blocks_function_generic <- function(blocks_internal, block_name){
  blocks_df <- covariates.frame(blocks_internal)
  colnames(blocks_df) <- block_name
  return(blocks_df)
} 

recode_function_generic <- function(blocks_untransformed, block_count){
  blocks_transformed <- cut(x = blocks_untransformed, 
                            breaks = quantile(x = blocks_untransformed, probs = seq(from = 0, to = 1, length.out = block_count + 1)),
                            include.lowest = TRUE)
  return(blocks_transformed)
}

## blocks <- c("age", "gender")

#' @export
declare_blocks <- function(blocks = NULL, recode = TRUE, recode_function = NULL, block_count = 5, 
                           multi_blocking_function = NULL, block_name = "block_variable"){
  
  ## set default recode_function
  if(is.null(recode_function)){
    recode_function <- function(blocks_untransformed, block_count) 
      recode_function_generic(blocks_untransformed = blocks_untransformed, block_count = block_count)
  }
  
  if(length(blocks) == 1 & recode == FALSE) {
    
    ##if(length(unique(covariates[, blocks])) > block_count)
    ##  stop("The variable you set in blocks has more categories than block_count, so it must be recoded. Set recode_variable = TRUE.")
    
    blocks_function <- function(covariates) blocks_function_generic(blocks_internal = blocks, block_name = block_name, covariates = covariates)
    
  } else if(length(blocks) > 1 | recode == TRUE) {
    
    ## set default multi_blocking_function
    if(is.null(multi_blocking_function)){
      multi_blocking_function <- function(blocks, covariates){
        df <- model.matrix(as.formula(paste("~", paste(blocks, collapse = "+"))), covariates = covariates)
        pca_first_component <- summary(princomp(df))$scores[,1]
        if(length(unique(pca_first_component)) < block_count)
          warning(paste("Not enough variation to make", block_count, "blocks. We made", length(unique(pca_first_component)), "blocks instead."))
        if(length(unique(pca_first_component)) > block_count){
          blocks_transformed <- recode_function(blocks_untransformed = pca_first_component, block_count = block_count)
        }  else {
          blocks_transformed <- pca_first_component
        }
        return(blocks_transformed)
      }
    }
    
    blocks_function <- function(covariates){      
      return(blocks_function_generic(blocks_internal = multi_blocking_function(blocks = blocks, covariates = covariates), block_name = block_name))
    }
      
  }
  
  return_object <- list(blocks_function = blocks_function, block_name = block_name, call = match.call())  
  class(return_object) <- "blocks_object"
  
  return(return_object)
  
}
