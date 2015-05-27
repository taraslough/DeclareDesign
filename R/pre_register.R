## take meta-data, design object, power object, and fake data object
## and turn each into a block of text and/or plots

## construct, based on one or more standard templates, an .Rmd markdown file
## (in the future also make a .Rnw knitr file)

## then, optionally, compile into either PDF, Word doc, or html

## each snippet of R code is constructed using match.call() from the relevant function
## and the associated print() or plot() function for that type of object, i.e.
## print(design(arguments)) spits out text from that R code snippet

## output to user is the .Rmd file, which includes R code snippets and template text


#' Create pre-registration document as a PDF, .Rmd, or .Rnw
#'
#' @param design A design object from the declare_design function.
#' @param title Title of the pre-registration
#' @param authors List of authors, consisting of vectors of two strings -- one for the author's name and one for her affiliation. e.g. \code{list(c("Alan Gerber", "Yale University"), c("Donald Green", "Columbia University"))}. Optionally, a third item in the vector can be the text for a footnote with author contact information.
#' @param file File name where object is saved in the current working directory.
#' @param type Type of document that is created, either \code{knitr} or \code{rmarkdown}, the default.
#' @param make_output Indicator for whether code for registration document is compiled into a PDF, Microsoft Word, or HTML document
#' @param output_format String indicating which type of output file is created, "pdf" (default), "word", or "html"
#' @param open_output Indicator for whether the output file is opened after it is compiled.
#' @return Filename and location where .Rmd or .Rnw and PDF file are saved.
#' @importFrom rmarkdown render
#' @export
pre_register <- function(design, title, authors, file = NULL,
                   type = "rmarkdown", ## options (will be) rmarkdown and knitr
                   make_output = TRUE,
                   output_format = "pdf", ## options are pdf, word, html
                   open_output = TRUE, ...){
  
  if(type != "rmarkdown")
    stop("Type must be 'rmarkdown' in the first version.")
  
  ## set up the markdown file
  ## (this will be the key part once we have the design/power/etc. objects)
  doc <- "testline1\n\ntestline2" ## test string
  rcode <- "runif(1)" ## test R code

  ## create temp file name if user does not supply one
  if(is.null(file)) 
    file <- tempfile("registration", fileext = ".Rmd")
  
  ## writes Rmd rmarkdown file
  sink(file)
  cat(create_header(x$title, x$authors, "ToDaY"))
  cat(doc)
  cat(create_code_snippet(rcode))
  sink()
  
  cat("\nRegistration document (markdown) saved in", file, "\n")
  
  ## compile Rmd into a PDF if requested
  if(make_output == TRUE){
    output_format_internal <- ifelse(output_format == "pdf", "pdf_document", 
                                     ifelse(output_format == "word", "word_document", 
                                            stop("Chosen output_format not supported.")))
    render(file, output_format_internal, quiet = TRUE)
    cat("\nRegistration document (PDF) saved in", 
        paste(substr(file, 1, nchar(file) - 4), ".pdf", sep = ""), "\n")
    
  }

  if(open_output == TRUE)
    system(paste("open ", substr(file, 1, nchar(file) - 4), ".pdf", sep = ""))
    
}

create_header <- function(title, authors, date){
  
  return(paste("---\ntitle: \"", title, "\"\noutput: pdf_document\n---\n\n", sep = ""))
  
}
  
create_code_snippet <- function(x, ## takes a character string
                                results = "asis", 
                                echo = FALSE) {
  return(paste("\n```{r, results='", results, "'}\n", x, "\n```\n", sep = ""))
}
  
  
