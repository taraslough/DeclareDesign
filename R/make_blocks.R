#' Make vector for blocked random assignment
#'
#' @param variable_name The name of your new variable 
#' @param linear_mean If the variable is linear 
#' @export
covariate <- covariates_to_block_on[1]


make_blocks <-
  function(covariates_to_block_on,
           design_object,
           covariate_matrix) {
             require(blockTools)
             
             
             block_variables <- data.frame(sapply(
               covariates_to_block_on,function(covariate){
               covariate_class <- class(covariate_matrix[,covariate])
               if(!covariate_class%in%c("integer","numeric")){
                 covariate_matrix[,covariate] <- as.integer(covariate_matrix[,covariate])
               }
               return(unlist(covariate_matrix[,covariate]))
               
             }))
             
             block_variables$ID <- 1:dim(block_variables)[1]
             
             blocks <- blockTools::block(data = block_variables,
                                         n.tr = design_object$num_arms,
                                         id.vars = "ID",
                                         block.vars = covariates_to_block_on)
             
             block_assignment <- subset(blockTools::assignment(blocks)[[1]]$`1`)
             
             block_assignment <- block_assignment[,-which(names(block_assignment)=="Max Distance")]
             
             names(block_assignment) <- design_object$condition_names
             
             reshaped_assignment <- data.frame(
               ID = unlist(sapply(1:dim(block_assignment)[2],function(i){
                 block_assignment[,i][!is.na(block_assignment[,i])]
               })),
               condition = as.integer(as.factor(unlist(sapply(1:dim(block_assignment)[2],function(i){
                 rep(names(block_assignment)[i],
                     length(block_assignment[,i][!is.na(block_assignment[,i])]))
                 }))))
               
             )
             
             block_indicator <- merge(block_variables,
                                      reshaped_assignment,
                                      by = "ID")$condition
             
             return(block_indicator)

           }


# Demo
design_7 <- declare_design(clust_var=rep(letters, times=1:26), 
                           block_var = rep(rep(1:13, each=2), times=1:26), 
                           num_arms=4)

covariate_object_1 <- make_covariates(
  X1 = declare_DGP(),
  X2 = declare_DGP(),
  event = declare_DGP(binary_probability = .5,
                      binary_categories = c("happened","did not")),
  income = function()rnorm(n = design_7$N,mean = 0,sd = 1),
  count = function()rpois(n = design_7$N,lambda = 30),
  party_id = declare_DGP(
    multinomial_probabilities = c(.4,.4,.2),
    multinomial_categories = c("D","R","I")),
  design_object = design_7
)

make_blocks(covariates_to_block_on = c("event","income","count","party_id"),
            design_object = design_7,
            covariate_matrix = covariate_object_1$make_X_matrix()
            )









