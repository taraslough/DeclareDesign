rm(list=ls())
library(testthat)
library(DeclareDesign)

context("user-provided data")

test_that("test user provided data can be re-sampled correctly", {
  
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
  
  pop_1 <- declare_population(
    data = user_data)
  
  pop_2 <- declare_population(
    data = user_data,
    super_population = T,
    size = c(10,5,2),
    level_IDs = c("indiv_ID","city_ID","region_ID"))
  
  # Fixed user data
  head(draw_population(population = pop_1))
  
  # Bootstrapped user data
  head(draw_population(population = pop_2))
  
  # Changing the size argument of bootstrapping
  pop_2$population(size = c(8,4,2))
  
})
