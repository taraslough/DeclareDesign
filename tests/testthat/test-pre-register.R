context("Check that the pre_register function works")

rm(list=ls())

test_that("pre-registration runs", {
  
  pre_register(design, data, analysis, 
               registration_title = "An RCT in Another Country", registration_authors = "Graeme Blair", 
               registration_description,
               random.seed = 42, dir = "~/downloads/testing", type = "rmarkdown",
               make_output = TRUE, keep_tex = TRUE, output_format = "pdf", open_output = TRUE)
  
})
