## plan for the function:

## take meta-data, design object, power object, and fake data object
## and turn each into a block of text and/or plots

## construct, based on one or more standard templates, an .Rmd markdown file
## (in the future also make a .Rnw knitr file for those that prefer Knitr)

## the user can then edit this file manually and compile on their own

## the file will be constructed by creating markdown snippets using the 
## calls to the design, power, and fake data functions rather than including
## those objects, so the code runs again each time

## then, optionally, compile into either PDF, Word doc, or html

## each snippet of R code is constructed using match.call() from the relevant function
## and the associated print() or plot() function for that type of object, i.e.
## print(design(arguments)) spits out text from that R code snippet

## output to user is (1) the .Rmd file, which includes R code snippets and template text
## (2) the "output" = compiled file in pdf, word, or html format


#' Create pre-registration document
#'
#' @param design A design object from the declare_design function.
#' @param data A data object from the make_y function.
#' @param analysis An analysis object from the declare_analysis function, or a list of analysis objects.
#' @param registration_title Title of the pre-registration.
#' @param registration_authors List of authors, consisting of vectors of two strings -- one for the author's name and one for her affiliation. e.g. \code{list(c("Alan Gerber", "Yale University"), c("Donald Green", "Columbia University"))}. Optionally, a third item in the vector can be the text for a footnote with author contact information.
#' @param registration_abstract General description of the experiment.
#' @param random.seed Random seed to ensure reproducibility of the design.
#' @param file File name where object is saved.
#' @param type Type of document that is created, either \code{knitr} or \code{rmarkdown}, the default.
#' @param check_registration Indicates whether the design is evaluated for consistency with declared analyses.
#' @param make_output Indicator for whether code for registration document is compiled into a PDF, Microsoft Word, or HTML document
#' @param output_format String indicating which type of output file is created, "pdf" (default), "word", or "html"
#' @param open_output Indicator for whether the output file is opened after it is compiled.
#' @param ... Other options for the render() command to create the output file from the markdown code.
#' @return Filename and location where .Rmd or .Rnw and PDF file are saved.
#' @importFrom rmarkdown render
#' @export
pre_register <- function(design, covariates, potential_outcomes, analysis, 
                         registration_title, registration_authors, registration_abstract,
                         random.seed = 42, dir = getwd(), temp_dir = FALSE, type = "rmarkdown",
                         check_registration = TRUE,
                         make_output = TRUE, output_format = "pdf", keep_tex = FALSE, 
                         save_r_code = FALSE,
                         open_output = TRUE, ...){
  
  if(class(analysis) != "list")
    analysis <- list(analysis)
  if(class(analysis) != "list" & class(analysis) != "analysis")
    stop("Analysis must be either a list of analysis objects or a single object created by declare_analysis.")
  
  if(check_registration == TRUE)
    check_registration(design = design, analysis = analysis, covariates = covariates, 
                       potential_outcomes = potential_outcomes)
  
  if(type != "rmarkdown")
    stop("Type must be 'rmarkdown' in the first version.")
  
  ## create temp dir if user does not supply one
  if(temp_dir == TRUE)
    dir <- tempdir()
  
  file <- paste("registration-", format(Sys.time(), "%d-%b-%Y-%Hh%Mm%Ss"), sep = "")
  
  template <- readLines(system.file("tex", "egap_registration_template.tex", package = "registration"))
  
  ## write EGAP template to directory
  writeLines(template, con = paste(dir, "/egap_registration_template.tex", sep = ""))
  
  ## writes Rmd rmarkdown file
  ## send it a set of character objects
  i <- 1
  cat_doc(
    title_header(title = registration_title, authors = registration_authors, 
                 abstract = registration_abstract, keep_tex = keep_tex),
    code_snippet("library(registration) \n library(xtable)"),
    code_snippet("## set fixed random seed for registration reproducibility\n\nset.seed(", 
                 random.seed, ")"),
    code_snippet("cov <- ", covariates$call, "\n\n", 
                 "po <- ", potential_outcomes$call, "\n\n", 
                 "mock <- make_data(potential_outcomes = po, covariates = cov)", "\n\n",
                 "design <- ", design$call, "\n\n", 
                 paste(sapply(1:length(analysis), 
                              function(x) paste0("analysis_", x, " <- ", list(analysis[[x]]$call), "\n\n")), 
                       collapse = ""),
                 echo = T),
    code_snippet(paste(sapply(1:length(analysis), 
                              function(x) paste0("mock[, analysis_treatment_variable(analysis_", x, 
                                                 ")] <- assign_treatment(design)\n\n",
                                                 "mock[, analysis_outcome_variable(analysis_", x, ")] <- observed_outcome(outcome = analysis_outcome_variable(analysis_", x, "), treatment_assignment = 'Z', design = design, data = mock) \n")), collapse = "")),
    tex_header("Hypotheses", 1),
    "Please write your hypotheses here. Be sure to explain each declared analysis.",
    tex_header("Experimental Design", 1),
    code_snippet("summary(design)"),
    code_snippet("print(xtable(table(design$ra_fun()), caption = \"Example Random Assignment\"), include.colnames = FALSE, comment = FALSE)"),
    "Please describe your experimental conditions and randomization protocol.",
    tex_header("Power analysis", 2),
    code_snippet(paste0("cat(\"The power of analysis ", 1:length(analysis), 
                        " is \", get_power(design = design, analysis = analysis_", 1:length(analysis), 
                        ", data = mock), \". \", sep = \"\")\n\n")),
    tex_header("Results", 1),
    paste(sapply(1:length(analysis), function(x) { 
      paste(tex_header(paste("Simulated results for analysis", x), 2), "\n",
            code_snippet("print(xtable(run_analysis(analysis_", x, 
                         ", data = mock), caption = \"Analysis ", x,
                         " Results with Simulated Data\"), comment = FALSE)"), "\n\n", collapse = "")   
    }), collapse = ""),
    filename = paste(dir, "/", file, ".Rmd", sep = "")
  )
  
  cat("\nRegistration raw document (markdown .Rmd file) saved in ", dir, "/", file, ".Rmd\n", sep = "")
  
  ## compile Rmd into a PDF or Word doc if requested
  if(make_output == TRUE){
    output_format_internal <- ifelse(output_format == "pdf", "pdf_document", 
                                     ifelse(output_format == "doc", "word_document", 
                                            stop("Chosen output_format not supported.")))
    input <- paste(dir, "/", file, ".Rmd", sep = "")
    render(input = input, output_format_internal, quiet = TRUE, ... = ...)
    
    if(keep_tex == TRUE)
      cat("\nRegistration raw document (.tex file) saved in ", dir, "/", file, ".tex\n", sep = "")
    
    cat("\nRegistration output document (PDF file) saved in", 
        paste(dir, "/", file, ".pdf", sep = ""), "\n")
    
  }
  
  if(save_r_code == TRUE)
    knitr::purl(input = paste(dir, "/", file, ".Rmd", sep = ""),
                output = paste(dir, "/", file, ".R", sep = ""))
  
  ## open output file (i.e. PDF) if requested
  if(open_output == TRUE)
    system(paste("open ", dir, "/", file, ".pdf", sep = ""))
  
  cat("\n")
  
}

cat_doc <- function(..., filename){
  ddd <- list(...)
  sink(filename)
  for(i in 1:length(ddd))
    cat(ddd[[i]], "\n\n")
  sink()
}

tex_header <- function(title, level){
  if(level==1)
    return(paste(title, "\n==="))
  else if(level==2)
    return(paste(title, "\n--"))
} 

title_header <- function(title = NULL, authors = NULL, abstract = NULL, keep_tex = FALSE){
  
  ##return(paste("---\ntitle: \"", title, "\"\noutput: pdf_document\n---\n\n", sep = ""))
  
  ##abstract: |
  ##  This is the abstract.
  
  ##It consists of two paragraphs.
  
  authors.text <- paste("-", authors[1], "\n")
  if(length(authors) > 1) {
    for(i in 2:length(authors))
      authors.text <- paste(authors.text, paste("- ", authors[i], "\n", sep = ""), sep = "")
  }
  
  return(paste("---\ntitle: \"", title, 
               "\"\nauthor: \n", 
               authors.text,
               ifelse(is.null(abstract), "", paste("abstract: |\n", abstract, "\n")),
               "date: \"`r format(Sys.time(), \'%d %B %Y\')`\"",
               "\noutput:\n  pdf_document:\n    toc: yes\n    template: ./egap_registration_template.tex\n",
               ifelse(keep_tex, "    keep_tex: true\n", ""),
               "---\n\n", 
               sep = ""))
  
}

code_snippet <- function(..., ## takes a character string
                         results = "asis", 
                         echo = FALSE,
                         tidy = TRUE) {
  return(paste("```{r, echo =", echo, ", tidy =", tidy, ", results ='", results, "'}\n", 
               paste(unlist(list(...)), collapse = ""), 
               "\n```", sep = ""))
}


