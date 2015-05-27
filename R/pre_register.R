#' Create pre-registration object to document a design, power analysis, and experiment analyses
#'
#' @param design A design object from the declare_design function.
#' @param title Title of the pre-registration
#' @param authors List of authors, consisting of vectors of two strings -- one for the author's name and one for her affiliation. e.g. \code{list(c("Michael LaCour", "UCLA"), c("Donald Green", "Columbia University"))}. Optionally, a third item in the vector can be the text for a footnote with author contact information.
#' @return \code{pre_registration} object that can be exported.
#' @export
#' @examples
#' export(design.object, compile_pdf = TRUE, open_pdf = TRUE)
pre_register <- function(design,  ## object created by declare_design function
                         title, ## text of title of the registration in the PDF
                         authors ## list object of authors in order, 
                                 ## with each being a two-item character string, 
                                 ## name then affiliation
                          ) {
  
  design <- c()
  
  pre_registration_object <- list(design = design, title = title, authors = authors)
  
  class(pre_registration_object) <- "pre_registration"
  return(pre_registration_object)
  
}

#' @export
export.pre_registration <- function(x, file, type = "rmarkdown", compile_PDF = TRUE, 
                                    open_pdf = TRUE, ...){
  
  if(type != "knitr" & type != "rmarkdown")
    stop("Type must be 'knitr' or 'rmarkdown'.")
  
  ## set default text for registration doc
  doc <- "what"

  ## create temp file name
  if(!exists(file_name)) 
    file_name <- tempfile("registration", fileext = ".Rmd")
  
  ## writes Rmd rmarkdown file
  sink(file.path)
  cat("---\ntitle: \"", title, "\"\noutput: pdf_document\n---\n", doc, sep = "")
  sink()
  
  ## compile Rmd into a PDF if requested
  if(compile_pdf == TRUE) {
    if(type == "rmarkdown")
      render(file.path, "pdf_document")
    else if(type == "knitr")
      knit(file.path)
  }
  
  if(open_pdf == TRUE)
    system(paste("open ", substr(file.path, 1, nchar(file.path) - 4), ".pdf", sep = ""))
  
  return(file_name)
  
}
  
  
  
  
