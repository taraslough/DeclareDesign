#' @export
declare_strata <- function(strata = NULL, recode = TRUE, recode_function = NULL, strata_count = 5, clusters = NULL,
                           strata_function = NULL, strata_name = "strata_variable"){
  
  return_object <- declare_blocks(blocks = strata, recode = recode, recode_function = recode_function, block_count = strata_count, 
                                  clusters = clusters,
                                  blocks_function = strata_function, block_name = strata_name)
  names(return_object) <- c("strata_function", "strata_name", "call")
  class(return_object) <- "strata"
  return(return_object)
}
