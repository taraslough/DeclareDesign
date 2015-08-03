

blocks_function_generic <- function(blocks_internal, block_name, sample){
  x <- sample[,blocks_internal]
  n_digits <- nchar(as.character(length(unique(x))))
  x <- paste0("block_",sprintf(paste0("%0",n_digits,"d"),(as.numeric(as.factor(x)))))
  blocks_df <- data.frame(x, stringsAsFactors = FALSE)
  colnames(blocks_df) <- block_name
  return(blocks_df)
} 

recode_function_generic <- function(blocks_untransformed, block_count){
  blocks_transformed <- cut(x = blocks_untransformed, 
                            breaks = quantile(x = blocks_untransformed, 
                                              probs = seq(from = 0, to = 1, 
                                                          length.out = block_count + 1)),
                            include.lowest = TRUE)
  return(blocks_transformed)
}

multi_blocks_function_generic <- function(blocks, sample, block_name = block_name, block_count = block_count, recode_function = recode_function){
  
  df <- model.matrix(as.formula(paste("~", paste(blocks, collapse = "+"))), data = sample)
  pca_first_component <- summary(princomp(df))$scores[,1]
  
  if(length(unique(pca_first_component)) < block_count)
    warning(paste("Not enough variation to make", block_count, 
                  "blocks. We made", length(unique(pca_first_component)), "blocks instead."))
  
  if(length(unique(pca_first_component)) > block_count){
    blocks_transformed <- recode_function(blocks_untransformed = 
                                            pca_first_component, block_count = block_count)
  }  else {
    blocks_transformed <- pca_first_component
  }
  
  n_digits <- nchar(as.character(length(unique(blocks_transformed))))
  blocks_transformed <- paste0("block_",sprintf(paste0("%0",n_digits,"d"),
                                                (as.numeric(as.factor(blocks_transformed)))))
  blocks_df <- data.frame(blocks_transformed, stringsAsFactors = FALSE)
  colnames(blocks_df) <- block_name
  
  return(blocks_df)
}

#' Declare the data-generating process of a variable
#'
#' @param blocks A character vector describing the variables in the sample dataframe that will be used for blocking.
#' @param recode logical.  If TRUE (the default), principal components analysis used to make equally sized blocks, where the number of blocks is defined by block_count
#' @param recode_function A user-supplied function that creates blocks. If NULL (the default), principal components will be used to make blocks.
#' @param block_count The total number of blocks.  If the blocking variables imply fewer than this number, nothing changes. If the variables supplied in blocks imply more possible categories, then the variable is recoded (if recode=TRUE).
#' @param clusters desc
#' @param blocks_function Function to take multiple variables and construct blocks from them. Must take blocks, sample, and block_name and return
#' @param block_name A character string that gives the name of the blocking variable.
#' @export
declare_blocks <- function(blocks = NULL, recode = TRUE, recode_function = NULL, block_count = 5, clusters = NULL,
                           blocks_function = NULL, block_name = "block_variable"){
  
  if(!is.null(blocks) & !is.null(blocks_function)){
    stop("Please do not specify both the blocks argument and a user-defined blocks_function.")
  }
  
  ## set default recode_function
  if(is.null(recode_function)){
    recode_function <- function(blocks_untransformed, block_count){
      recode_function_generic(blocks_untransformed = blocks_untransformed, block_count = block_count)
    } 
  }
  
  if(length(blocks) == 1 & recode == FALSE) {
    
    blocks_function <- function(sample){
      if(length(unique(sample[, blocks])) > block_count)
        stop("The variable you set in blocks has more categories than block_count, so it must be recoded. Set recode_variable = TRUE.")
      blocks_function_generic(blocks_internal = blocks, block_name = block_name, sample = sample)
    }
    
  } else if(length(blocks) > 1 | recode == TRUE) {
    
    ## set default multi_blocking_function
    if(is.null(blocks_function)){
      
      blocks_function <- function(sample){      
        multi_blocks_function_generic(blocks = blocks, sample = sample, block_name = block_name, block_count = block_count, recode_function = recode_function)
      }
    }
  } 
  
  return_object <- list(blocks_function = blocks_function, block_name = block_name, call = match.call())  
  class(return_object) <- "blocks"
  
  return(return_object)
  
}

#' @export
summary.blocks <- function(object, ...){
  cat("This is a description of blocks. Not implemented yet.")
}


###
# N <- 100
# gender <- sample(c("M", "F"), N, replace=TRUE)
# party <- sample(c("D", "R", "I"),N, replace=TRUE)
# lincome <- rnorm(N, 5, 2)
# 
# 
# df <- data.frame(gender, lincome, party)
# 
# 
# blocks <- declare_blocks(blocks = "gender", recode = TRUE, block_count = 10)
# table(blocks$blocks_function(sample = df))
# 
# table(blocks$blocks_function(sample = df)[,1], gender)
# table(blocks$blocks_function(sample = df)[,1], party)


