context("Check that the pre_register function works")

rm(list=ls())

test_that("pre-registration runs", {
  
  pre_register(design, data, analysis, 
               registration_title = "An RCT in Another Country", 
               registration_authors = "Graeme Blair", 
               registration_abstract,
               random.seed = 42, type = "rmarkdown",
               make_output = TRUE, keep_tex = TRUE, output_format = "pdf", open_output = TRUE)
  
})
