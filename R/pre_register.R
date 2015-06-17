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
#' @param make_output Indicator for whether code for registration document is compiled into a PDF, Microsoft Word, or HTML document
#' @param output_format String indicating which type of output file is created, "pdf" (default), "word", or "html"
#' @param open_output Indicator for whether the output file is opened after it is compiled.
#' @param ... Other options for the render() command to create the output file from the markdown code.
#' @return Filename and location where .Rmd or .Rnw and PDF file are saved.
#' @importFrom rmarkdown render
#' @export
pre_register <- function(design, data, analysis, 
                         registration_title, registration_authors, registration_abstract,
                         random.seed = 42, dir = getwd(), temp.dir = FALSE, type = "rmarkdown",
                         make_output = TRUE, output_format = "pdf", keep_tex = FALSE, 
                         open_output = TRUE, ...){
  
  if(type != "rmarkdown")
    stop("Type must be 'rmarkdown' in the first version.")
  
  ## master loop of analyses
  ##power <- list()
  ##for(a in length(analysis)){
  
  ## need print of power of analysis and then analysis
  
  ##power[[a]] <- get_power(analysis = analysis[[a]], data = data, design = design)
  
  ##}
  
  ## create temp dir if user does not supply one
  if(temp.dir == TRUE)
    dir <- tempdir()
  
  file <- paste("registration-", format(Sys.time(), "%d-%b-%Y-%Hh%Mm%Ss"), sep = "")
  
  template <- readLines(system.file("tex", "egap_registration_template.tex", package = "registration"))
  
  ## write EGAP template to directory
  writeLines(template, con = paste(dir, "/egap_registration_template.tex", sep = ""))
  
  ## writes Rmd rmarkdown file
  ## send it a set of character objects
  cat_doc(
    title_header(title = registration_title, authors = registration_authors, keep_tex = keep_tex),
    code_snippet("## set fixed random seed for registration reproducibility\n\nset.seed(", 
                 random.seed, ")"),
    tex_header("Introduction", 1),
    registration_abstract,
    tex_header("Hypotheses", 1),
    tex_header("Experimental Design", 1),
    tex_header("Results", 1),
    ##code_snippet(analysis$call)
    
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

title_header <- function(title = NULL, authors = NULL, keep_tex = FALSE){
  
  ##return(paste("---\ntitle: \"", title, "\"\noutput: pdf_document\n---\n\n", sep = ""))
  
  ##abstract: |
  ##  This is the abstract.
  
  ##It consists of two paragraphs.
  
  authors.text <- paste("-", authors[1], "\n")
  if(length(authors) > 1) {
    for(i in 2:length(authors))
      authors.text <- paste(authors.text, paste("-", authors[i], "\n"))
  }
  
  return(paste("---\ntitle: \"", title, 
               "\"\nauthor: \n", 
               authors.text,
               "date: \"`r format(Sys.time(), \'%d %B %Y\')`\"",
               "\noutput:\n  pdf_document:\n    template: ./egap_registration_template.tex\n",
               ifelse(keep_tex, "    keep_tex: true\n", ""),
               "---\n\n", 
               sep = ""))
  
}

code_snippet <- function(..., ## takes a character string
                         results = "asis", 
                         echo = FALSE) {
  return(paste("```{r, echo =", echo, ", results='", results, "'}\n", 
               paste(unlist(list(...)), collapse = ""), 
               "\n```", sep = ""))
}


