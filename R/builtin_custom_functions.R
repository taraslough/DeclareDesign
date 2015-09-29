cluster_function_generic <- function(clusters_internal, cluster_name, sample){
  x <- sample[,clusters_internal]
  n_digits <- nchar(as.character(length(unique(x))))
  x <- paste0("cluster_",sprintf(paste0("%0",n_digits,"d"),(as.numeric(as.factor(x)))))
  cluster_df <- data.frame(x, stringsAsFactors = FALSE)
  colnames(cluster_df) <- cluster_name
  return(cluster_df)
} 


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
