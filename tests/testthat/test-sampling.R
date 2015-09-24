
rm(list=ls())
library(testthat)
library(experimentr)

context("Assignment and probability functions")

test_that("test assignment and probability functions", {
  
  pop <- declare_population(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(100000, 2000))
  
  pop_draw <- draw_population(population = pop)
  
  smpl_complete <- declare_sampling(prob = .02)
  
  smpl_draw_complete <- draw_sample(smpl_complete, data = pop_draw)
  
  smpl_stratified <- declare_sampling(prob = .02, strata = "villages_id")
  
  smpl_draw_stratified <- draw_sample(smpl_stratified, data = pop_draw)
  
  
})
