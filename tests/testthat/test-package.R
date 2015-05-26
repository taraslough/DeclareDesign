context("Check that basic code works")

rm(list=ls())

test_that("1=1", {
   expect_equivalent(1, 1)
})
