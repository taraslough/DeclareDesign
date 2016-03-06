rm(list=ls())
library(testthat)
library(DeclareDesign)

context("custom population functions")

test_that("test that custom population generation functions work", {
  
  # Very simple custom GDP function
  
  my_DGP <- function(size){
    data <- data.frame(matrix(data = rnorm(size^2),nrow = size))
    names(data) <- paste0("X",1:ncol(data))
    return(data)
  }
  
  declare_population(size = 10,custom_population_function = my_DGP)$population()
  
  # User-provided data, custom resampling function
  
  user_data <- declare_population(
    indiv = list(
      income = "rnorm(n_)",
      age = "rpois(n_,30)"
    ),
    city = list(
      city_educ_mean = "rnorm(n = n_, mean = 100, sd = 10)",
      city_educ_sd = "rgamma(n = n_, shape = 2, rate = 2)"
    ),
    region = list(),
    make_unique_ID = T,
    size = c(1000,50,20)
  )$population()
  
  my_pop_and_data_function <- function(size,data){
    N <- nrow(data)
    data[sample(1:N,size,TRUE),]
  }
  
  user_pop_1 <- declare_population(
    custom_population_function = my_pop_and_data_function,
    data = user_data,
    size = 1000,
    resample_data = T)
  
  user_pop_1$population(size = 10)
  
})
