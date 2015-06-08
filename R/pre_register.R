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
#' @param registration_description General description of the experiment.
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
                         registration_title, registration_authors, registration_description,
                         random.seed = 42, file = NULL, type = "rmarkdown",
                         make_output = TRUE, output_format = "pdf", open_output = TRUE, ...){
  
  if(type != "rmarkdown")
    stop("Type must be 'rmarkdown' in the first version.")
  
  ## set up the markdown file
  ## (this will be the key part once we have the design/power/etc. objects)
  doc <- "testline1\n\ntestline2" ## test string
  rcode <- "runif(1)" ## test R code
  
  ## master loop of analyses
  ##power <- list()
  ##for(a in length(analysis)){
    
    ## need print of power of analysis and then analysis
    
    ##power[[a]] <- get_power(analysis = analysis[[a]], data = data, design = design)
    
  ##}
  
  ## create temp file name if user does not supply one
  if(is.null(file)) 
    file <- tempfile(paste("registration-", format(Sys.time(), "%d-%b-%Y-%H:%M:%S"), sep = ""), 
                     fileext = ".Rmd")
  
  ## writes Rmd rmarkdown file
  sink(file)
  cat(create_header(registration_title, registration_authors, date()))
  cat(create_code_snippet(paste("## set fixed random seed for registration reproducibility\n\nset.seed(", 
                                random.seed, ")", sep = "")))
  cat(doc)
  cat(create_code_snippet(rcode))
  sink()
  
  cat("\nRegistration raw document (markdown .Rmd file) saved in", file, "\n")
  
  ## compile Rmd into a PDF if requested
  if(make_output == TRUE){
    output_format_internal <- ifelse(output_format == "pdf", "pdf_document", 
                                     ifelse(output_format == "doc", "word_document", 
                                            stop("Chosen output_format not supported.")))
    render(file, output_format_internal, quiet = TRUE, ... = ...)
    cat("\nRegistration output document (PDF file) saved in", 
        paste(substr(file, 1, nchar(file) - 4), ".pdf", sep = ""), "\n")
    
  }
  
  ## open output file (i.e. PDF) if requested
  if(open_output == TRUE)
    system(paste("open ", substr(file, 1, nchar(file) - 4), ".pdf", sep = ""))
  
  cat("\n")
  
}

create_header <- function(title, authors, date){
  
  return(paste("---\ntitle: \"", title, "\"\noutput: pdf_document\n---\n\n", sep = ""))
  
}

create_code_snippet <- function(x, ## takes a character string
                                results = "asis", 
                                echo = FALSE) {
  return(paste("\n```{r, echo =", echo, ", results='", results, "'}\n", x, "\n```\n", sep = ""))
}


