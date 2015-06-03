rm(list = ls())

# make_data() -------------------------------------------------------------

# This function needs to handle four cases:
# 1. No outcome data provided, covariates provided
#    - user provides treatment formula (y ~ Y0 + Z1*.2 + Z2 *. 3 + Z1 * X1 * .05) 
#    - user provides baseline formula (Y0 ~ 10 + X1 * .2 + X2*.8 + X3 * .1 + X1 * X3 * .01)
#    - user provides covariate matrix 
#    - user provides residual variance 
#         - function stitches together the two formulae, generates a vector 
#           of outcomes for each unit that is as long as the number of treatments
# 2. No outcome data provided, no covariates provided
#    - user provides parameters for covariates (clustering, DGP, etc.)
#    - user provides residual variance (epsilon)
#    - user provides treatment formula
#    - user provides baseline formula
#         - function generates covariate matrix, then uses combination of 
#           baseline and treatment formulae to generate
#           potential outcomes 
# 3. Outcome data provided, no covariates
#    - user provides residual variance 
#    - user provides treatment formula
#    - user provides vector of baseline outcomes
#    - no covariates possible (too difficult to back them out)
#         - function applies treatment formula over the vector of outcomes, 
#           generating matrix of potential outcomes
# 4. Outcome data and covariates provided 
#    - this is similar to case 3: the user provides the treatment formula,
#      the covariate matrix (for treatment by cov interactions), the baseline
#      outcomes (residual variance can be set to 0 or added as part of treatment)
#         - function generates treatment effects using treatment fomula



# Below I demonstrate how the code would work under case 1: once we have
# worked this out, it is a small step to code cases 3 & 4. 
# The trickier part is figuring out how to allow users to generate all 
# of the covariates 

# Function starts here ----------------------------------------------------




make_data <- function(
     treatment.formula,       
     baseline.formula = NULL,  
     covariate.matrix = NULL, 
     outcome.vector = NULL,
     residual.variance = 1
     # ... I will add in the other args later
     
){

          # Check if it's case 1:

          if(!is.null(covariate.matrix)&
             is.null(outcome.vector)
          ){

                  # Check that the baseline is nested in the treatment formula
                  if(!grepl(pattern = as.character(baseline.formula)[2],
                            x       = as.character(treatment.formula)[3]))
                       stop("Treatment formula should contain outcome of baseline formula.")
                    
                  # Check that the variable names in the baseline formula match those in the
                  # covariate matrix
                  if(FALSE%in%(all.vars(baseline.formula)[-1]%in%colnames(covariate.matrix)))
                       stop("Variables in baseline formula should match the column names in the covariate matrix.")
                  
                  # Get the names of the treatment, removing any of the baseline variables
                  treat.names  <- all.vars(treatment.formula
                                          )[!all.vars(treatment.formula)%in%all.vars(baseline.formula)][-1]
                  
                  # Get the number of treatments 
                  num.treats   <- length(treat.names)
                  
                  # Get an indicator matrix for the treatments 
                  treat.mat    <- diag(num.treats)
                  colnames(treat.mat) <- treat.names
                  
                  # Get the full formula for the potential outcomes
                  full.formula <- 
                       gsub(pattern     = as.character(baseline.formula)[2],
                            replacement = as.character(baseline.formula)[3],
                            x           = as.character(treatment.formula)[3])
                                   
                  # Make a function that generates potential outcomes as a function of 
                  # all of the variables (treatment assignment, covariates) and some normal noise
                  gen.outcome  <- eval(parse(text = paste0(
                       "function(slice){y <- with(slice,{",full.formula,"}) + rnorm(1,0,residual.variance);return(y)}"
                       )))
                  
                  # Make another function that applies the gen.outcome function across 
                  # the treatment condition indicators, generating a vector of outcomes for 
                  # each observation, conditional on assignment
                  each.treat   <- function(cov.slice){apply(
                       X      = treat.mat,
                       MARGIN = 1,
                       FUN    = function(treat.mat.slice){
                            gen.outcome(slice = as.data.frame(t(unlist(c(
                                 treat.mat.slice,cov.slice)
                            ))))
                       }
                  )}
                  
                  # Apply that function through the covariate matrix to get the potential outcomes
                  outcomes     <- data.frame(t(apply(
                       X      = covariate.matrix,
                       MARGIN = 1,
                       FUN    = each.treat
                       )))
                  
                  # Give outcomes user-defined names 
                  names(outcomes) <- paste0(as.character(treatment.formula)[2],".",treat.names)
                  
                  
                  data         <- data.frame(outcomes,covariate.matrix)
                  
                  
                  return(data)
                  
                  
             } # end of case 3
          
          
     } 


# Demo of make_data() under case 1 ----------------------------------------


# Set up a basic data structure
N <- 100
K <- 8
X <- as.data.frame(sapply(1:K,function(mu)rnorm(n = N,mean = mu,sd = 1)))
names(X) <- paste0("var",1:K)

# Define function arguments 

covariate.matrix <- X

outcome.vector <- NULL

residual.variance <- 1

# NOTE 3 things about the formulae: 
#    - 1) 'control' is generated as the Z0 treatment condition, coeff of 0
#    - 2) covariate interactions included in treatment.formula
#         - but in principle it wouldn't screw the code up if they are put in the baseline formula
#    - 3) baseline is 'nested' within the treatment formula
#         - I imagine you will be tempted to have this as one formula, 
#           but this would make it very difficult to do the potential outcome 
#           matrix because the function wouldn't know which variables are treatment
#           and which are covariates. I suggest this as the most flexible way to 
#           provide the argument functions
treatment.formula <- Y ~ Y0 + Z0*0 + Z1*.5 + Z2*.3 + Z1.Z2*.8 + var1*Z1*.7 + var2*Z1*.10

baseline.formula <- Y0 ~ 12 + var1*.5 + var2*.3 + var3*.8 + var4*-.4 + var5*-.7 + var6 * .3 +
  var7*.4 + var8*.1 + var1*var2*.05 + var7*var3*.02 


make_data(baseline.formula  = baseline.formula,
          treatment.formula = treatment.formula,
          covariate.matrix  = covariate.matrix,
          outcome.vector    = outcome.vector,
          residual.variance = 1 
          )
