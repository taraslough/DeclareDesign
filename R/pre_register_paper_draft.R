
#' Create pre-registration document
#' 
#' Description
#' 
#' @param design A design object from the declare_design function.
#' @param clusters what is it?
#' @param blocks what is it?
#' @param sample_frame what is it?
#' @param potential_outcomes what is it?
#' @param analysis what is it?
#' @param data A data object from the make_data function.
#' @param title Title of the pre-registration.
#' @param authors List of authors, consisting of vectors of two strings -- one for the author's name and one for her affiliation. e.g. \code{list(c("Alan Gerber", "Yale University"), c("Donald Green", "Columbia University"))}. Optionally, a third item in the vector can be the text for a footnote with author contact information.
#' @param affiliations what is it?
#' @param acknowledgements what is it?
#' @param abstract General description of the experiment.
#' @param random_seed Random seed to ensure reproducibility of the design.
#' @param dir what is it?
#' @param temp_dir what is it?
#' @param format what is it?
#' @param check_registration what is it?
#' @param make_output Indicator for whether code for registration document is compiled into a PDF, Microsoft Word, or HTML document
#' @param keep_tex what is it?
#' @param save_r_code what is it?
#' @param output_format String indicating which type of output file is created, "pdf" (default), "word", or "html"
#' @param open_output Indicator for whether the output file is opened after it is compiled.
#' @return what does it return?
#' @export
pre_register <- function(design, clusters = NULL, blocks = NULL, sample_frame = NULL, 
                         potential_outcomes = NULL, analysis = NULL, data = NULL,
                         title = NULL, authors = NULL, affiliations = NULL,
                         acknowledgements = NULL, abstract = NULL,
                         random_seed = 42, dir = getwd(), temp_dir = FALSE, format = "rmarkdown",
                         check_registration = TRUE,
                         make_output = TRUE, keep_tex = FALSE, save_r_code = FALSE, 
                         output_format = "pdf", open_output = TRUE){
  
  if(missing(design))
    stop("Please provide a design object created using the declare_design() function.")
  
  if(class(analysis) != "list")
    analysis <- list(analysis)
  if(class(analysis) != "list" & class(analysis) != "analysis")
    stop("Analysis must be either a list of analysis objects or a single object created by declare_analysis.")
  
  if(check_registration == TRUE)
    check_registration(design = design, analysis = analysis, sample_frame = sample_frame, 
                       potential_outcomes = potential_outcomes, blocks = blocks, clusters = clusters)
  
  pre_register_doc <- list(title_header(title = title, authors = authors, 
                                        abstract = abstract, keep_tex = keep_tex),
                           code_snippet("library(registration) \n library(xtable)"),
                           ifelse(!is.null(data), 
                                  code_snippet("load(\"pre_registration_data.RData\")"), ""),
                           code_snippet("## set fixed random seed for registration reproducibility\n\nset.seed(", 
                                        random_seed, ")"),
                           code_snippet("sample_frame <- ", sample_frame$call, "\n\n", 
                                        "potential_outcomes <- ", potential_outcomes$call, "\n\n", 
                                        ifelse(!is.null(clusters), paste0("clusters <- ", list(clusters$call)), ""), "\n\n", 
                                        ifelse(!is.null(blocks), paste0("blocks <- ", list(blocks$call)), ""), "\n\n", 
                                        "design <- ", design$call, "\n\n", 
                                        paste(sapply(1:length(analysis), 
                                                     function(x) paste0("analysis_", x, " <- ", list(analysis[[x]]$call), "\n\n")), 
                                              collapse = ""),
                                        echo = T),
                           code_snippet("mock <- make_data(sample_frame = sample_frame, potential_outcomes = potential_outcomes", 
                                        ifelse(!is.null(clusters), ", clusters = clusters", ""),
                                        ifelse(!is.null(blocks), ", blocks = blocks", ""), ")\n\n", 
                                        paste(sapply(1:length(analysis), 
                                                     function(x) paste0("mock[, analysis_treatment_variable(analysis_", x, 
                                                                        ")] <- assign_treatment(design, data = mock)\n\n",
                                                                        "mock[, analysis_outcome_variable(analysis_", x, 
                                                                        ")] <- observed_outcome(outcome = analysis_outcome_variable(analysis_", x, 
                                                                        "), treatment_assignment = 'Z', data = mock) \n")), collapse = "")),
                           tex_header("Hypotheses", 1),
                           "Please write your hypotheses here. Be sure to explain each declared analysis.",
                           tex_header("Experimental Design", 1),
                           code_snippet("summary(design)"),
                           code_snippet("simulations <- summary(simulate_experiment(design = design, analysis = analysis_1,
                                           sample_frame = sample_frame, potential_outcomes = potential_outcomes",
                                        ifelse(!is.null(clusters), ", clusters = clusters", ""),
                                        ifelse(!is.null(blocks), ", blocks = blocks", ""), "))", "\n\n",
                                        "print(xtable(simulations, caption = \"Power analysis of quantities of interest\"),
                                              include.rownames = FALSE, comment = FALSE)"),
                           code_snippet("print(xtable(with(mock, table(Z)), ",
                                        "caption = \"Example Random Assignment\"), include.colnames = FALSE, comment = FALSE)"),
                           "Please describe your experimental conditions and randomization protocol.",
                           tex_header("Power analysis", 2),
                           tex_header("Results", 1),
                           paste(sapply(1:length(analysis), function(x) { 
                             paste(tex_header(paste("Simulated results for analysis", x), 2), "\n",
                                   code_snippet("print(xtable(get_estimates(analysis_", x, 
                                                ", data = mock), caption = \"Analysis ", x,
                                                " Quantities of Interest with Simulated Data\"), comment = FALSE)", "\n\n",
                                                "print(xtable(get_estimates_model(analysis_", x, 
                                                ", data = mock), caption = \"Analysis ", x,
                                                " Results with Simulated Data\"), comment = FALSE)"), "\n\n", collapse = "")   
                           }), collapse = ""))
  
  output_document(doc = pre_register_doc, pre_registration_data = data, dir = dir, temp_dir = temp_dir, format = format, make_output = make_output, keep_tex = keep_tex,
                  save_r_code = save_r_code, output_format = output_format, open_output = open_output)
  
  return_object <- list(design = design, sample_frame = sample_frame, potential_outcomes = potential_outcomes, 
                        clusters = clusters, blocks = blocks, analysis = analysis, data = data,
                        title = title, authors = authors, affiliations = affiliations,
                        acknowledgements = acknowledgements, abstract = abstract, random_seed = random_seed,
                        pre_registration_date = date())
  
  structure(return_object, class = "pre_registration")
  
}

#' @export
print.pre_registration <- function(x, ...){
  return()
}

#' Create paper draft from a pre_registration
#'
#' @param pre_registration Object created by pre_register() function. The inputs to create a paper draft are extracted exactly from the pre-registered descriptions of the experiment.
#' @param data what is it?
#' @param dir what is it?
#' @param temp_dir what is it?
#' @param format Type of document that is created, either \code{knitr} or \code{rmarkdown}, the default.
#' @param make_output Indicator for whether code for registration document is compiled into a PDF, Microsoft Word, or HTML document
#' @param keep_tex what is it?
#' @param save_r_code what is it?
#' @param output_format String indicating which type of output file is created, "pdf" (default), "word", or "html"
#' @param open_output Indicator for whether the output file is opened after it is compiled.
#' @return what is it?
#' @export
draft_paper_from_pre_registration <- function(pre_registration, data, dir = getwd(), 
                                              temp_dir = FALSE, format = "rmarkdown",
                                              make_output = TRUE, keep_tex = FALSE, save_r_code = FALSE, 
                                              output_format = "pdf", open_output = TRUE){
  
  if(missing(pre_registration))
    stop("Please provide a pre_registration object created using the pre_register() function, or you can create a paper draft directly using the draft_paper() function without a pre_registration object.")
  
  warning("Note: if you edited the pre-registration document after the pre_register() function saved it, you should start your paper draft based on the code in the final pre-registration document you submitted to a repository.")
  
  if(missing(data))
    stop("To make a paper draft, you must provide a data argument.")
  
  draft_paper(design = pre_registration$design, clusters = pre_registration$clusters,
              blocks = pre_registration$blocks, sample_frame = pre_registration$sample_frame,
              potential_outcomes = pre_registration$potential_outcomes, analysis = pre_registration$analysis,
              title = pre_registration$title, authors = pre_registration$authors, 
              affiliations = pre_registration$affiliations, abstract = pre_registration$abstract,
              pre_registration_data = pre_registration$data, data = data,
              random_seed = pre_registration$random_seed, dir = dir, temp_dir = temp_dir, format = format,
              make_output = make_output, keep_tex = keep_tex, save_r_code = save_r_code,
              output_format = output_format, open_output = open_output)
  
}


#' Create paper draft
#' 
#' Description
#'
#' @param design A design object from the declare_design function.
#' @param clusters what is it?
#' @param blocks what is it?
#' @param sample_frame what is it?
#' @param potential_outcomes what is it?
#' @param analysis what is it?
#' @param pre_registration_data A data object from the make_data function.
#' @param title Title of the pre-registration.
#' @param authors List of authors, consisting of vectors of two strings -- one for the author's name and one for her affiliation. e.g. \code{list(c("Alan Gerber", "Yale University"), c("Donald Green", "Columbia University"))}. Optionally, a third item in the vector can be the text for a footnote with author contact information.
#' @param affiliations what is it?
#' @param acknowledgements what is it?
#' @param abstract General description of the experiment.
#' @param data what is it?
#' @param random_seed Random seed to ensure reproducibility of the design.
#' @param dir what is it?
#' @param temp_dir what is it?
#' @param format what is it?
#' @param make_output Indicator for whether code for registration document is compiled into a PDF, Microsoft Word, or HTML document
#' @param keep_tex what is it?
#' @param save_r_code what is it?
#' @param output_format String indicating which type of output file is created, "pdf" (default), "word", or "html"
#' @param open_output Indicator for whether the output file is opened after it is compiled.
#' @return what does it return
#' @export
draft_paper <- function(design, clusters = NULL, blocks = NULL, sample_frame = NULL, 
                        potential_outcomes = NULL, analysis = NULL, pre_registration_data = NULL,
                        title = NULL, authors = NULL, affiliations = NULL,
                        acknowledgements = NULL, abstract = NULL, data = NULL,
                        random_seed = 42, dir = getwd(), temp_dir = FALSE, format = "rmarkdown",
                        make_output = TRUE, keep_tex = FALSE, save_r_code = FALSE, 
                        output_format = "pdf", open_output = TRUE){
  
  if(missing(design))
    stop("Please provide a design object created using the declare_design() function.")
  
  if(class(analysis) != "list")
    analysis <- list(analysis)
  if(class(analysis) != "list" & class(analysis) != "analysis")
    stop("Analysis must be either a list of analysis objects or a single object created by declare_analysis.")
  
  paper_draft_doc <- list(title_header(title = title, authors = authors, 
                                       abstract = abstract, keep_tex = keep_tex, pre_register = FALSE),
                          code_snippet("library(registration) \n library(xtable)"),
                          ifelse(!is.null(pre_registration_data), 
                                 code_snippet("load(\"pre_registration_data.RData\")"), ""),
                          code_snippet("load(\"paper_data.RData\")"),
                          code_snippet("## set fixed random seed for paper reproducibility\n\nset.seed(", 
                                       random_seed, ")"),
                          code_snippet("sample_frame <- ", sample_frame$call, "\n\n", 
                                       "potential_outcomes <- ", potential_outcomes$call, "\n\n", 
                                       ifelse(!is.null(clusters), paste0("clusters <- ", list(clusters$call)), ""), "\n\n", 
                                       ifelse(!is.null(blocks), paste0("blocks <- ", list(blocks$call)), ""), "\n\n", 
                                       "design <- ", design$call, "\n\n", 
                                       paste(sapply(1:length(analysis), 
                                                    function(x) paste0("analysis_", x, " <- ", list(analysis[[x]]$call), "\n\n")), 
                                             collapse = ""),
                                       echo = T),
                          code_snippet("mock <- make_data(sample_frame = sample_frame, potential_outcomes = potential_outcomes", 
                                       ifelse(!is.null(clusters), ", clusters = clusters", ""),
                                       ifelse(!is.null(blocks), ", blocks = blocks", ""), ")\n\n", 
                                       paste(sapply(1:length(analysis), 
                                                    function(x) paste0("mock[, analysis_treatment_variable(analysis_", x, 
                                                                       ")] <- assign_treatment(design, data = mock)\n\n",
                                                                       "mock[, analysis_outcome_variable(analysis_", x, 
                                                                       ")] <- observed_outcome(outcome = analysis_outcome_variable(analysis_", x, 
                                                                       "), treatment_assignment = 'Z', data = mock) \n")), collapse = "")),
                          tex_header("Hypotheses", 1),
                          "Please write your hypotheses here. Be sure to explain each declared analysis.",
                          tex_header("Experimental Design", 1),
                          code_snippet("summary(design)"),
                          code_snippet("simulations <- summary(simulate_experiment(design = design, analysis = analysis_1,
                                           sample_frame = sample_frame, potential_outcomes = potential_outcomes, 
                                           clusters = clusters, blocks = blocks))", "\n\n",
                                       "print(xtable(simulations, caption = \"Power analysis of quantities of interest\"),
                                              include.rownames = FALSE, comment = FALSE)"),
                          code_snippet("print(xtable(with(mock, table(Z)), ",
                                       "caption = \"Example Random Assignment\"), include.colnames = FALSE, comment = FALSE)"),
                          "Please describe your experimental conditions and randomization protocol.",
                          tex_header("Power analysis", 2),
                          tex_header("Results", 1),
                          paste(sapply(1:length(analysis), function(x) { 
                            paste(tex_header(paste("Simulated results for analysis", x), 2), "\n",
                                  code_snippet("print(xtable(get_estimates(analysis_", x, 
                                               ", data = mock), caption = \"Analysis ", x,
                                               " Quantities of Interest with Simulated Data\"), comment = FALSE)", "\n\n",
                                               "print(xtable(get_estimates_model(analysis_", x, 
                                               ", data = mock), caption = \"Analysis ", x,
                                               " Results with Simulated Data\"), comment = FALSE)"), "\n\n", collapse = "")   
                          }), collapse = ""))
  
  
  output_document(doc = paper_draft_doc, pre_registration_data = data, 
                  data = data, dir = dir, temp_dir = temp_dir, format = format, 
                  make_output = make_output, keep_tex = keep_tex,
                  save_r_code = save_r_code, output_format = output_format, open_output = open_output)
  
}

#' @importFrom knitr purl
#' @importFrom rmarkdown render
output_document <- function(doc, pre_registration_data = NULL, data = NULL, type = "registration",
                            title = NULL, authors = NULL, affiliations = NULL, 
                            acknowledgements = NULL, abstract = NULL,
                            random_seed = 42, dir = getwd(), temp_dir = FALSE, format = "rmarkdown",
                            make_output = TRUE, keep_tex = FALSE, save_r_code = FALSE, 
                            output_format = "pdf", open_output = TRUE) {
  if(format != "rmarkdown")
    stop("Format must be 'rmarkdown' in the first version.")
  
  ## create temp dir if user does not supply one
  if(temp_dir == TRUE)
    dir <- tempdir()
  
  file <- paste(type, format(Sys.time(), "%d-%b-%Y-%Hh%Mm%Ss"), sep = "")
  
  template <- readLines(system.file("tex", "egap_registration_template.tex", package = "registration"))
  
  ## write EGAP template to directory
  writeLines(template, con = paste(dir, "/egap_registration_template.tex", sep = ""))
  
  ## if data is sent, save it
  if(!is.null(data))
    save(data, file = paste(dir, "/paper_data.RData", sep = ""))
  ##  save(data, file = paste(dir, "/", file, "_paper_data.RData", sep = ""))
  
  if(!is.null(pre_registration_data))
    save(pre_registration_data, file = paste(dir, "/pre_registration_data.RData", sep = ""))
  ##  save(pre_registered_data, file = paste(dir, "/", file, "_pre_registration_data.RData", sep = ""))
  
  ## writes Rmd rmarkdown file
  ## send it a set of character objects
  cat_doc(doc = doc, filename = paste(dir, "/", file, ".Rmd", sep = "") )
  
  cat("\nRaw document (markdown .Rmd file) saved in ", dir, "/", file, ".Rmd\n", sep = "")
  
  ## compile Rmd into a PDF or Word doc if requested
  if(make_output == TRUE){
    output_format_internal <- ifelse(output_format == "pdf", "pdf_document", 
                                     ifelse(output_format == "doc", "word_document", 
                                            stop("Chosen output_format not supported.")))
    input <- paste(dir, "/", file, ".Rmd", sep = "")
    render(input = input, output_format_internal, quiet = TRUE)
    
    if(keep_tex == TRUE)
      cat("\nRaw document (.tex file) saved in ", dir, "/", file, ".tex\n", sep = "")
    
    cat("\nOutput document (PDF file) saved in", 
        paste(dir, "/", file, ".pdf", sep = ""), "\n")
    
  }
  
  if(save_r_code == TRUE){
    purl(input = paste(dir, "/", file, ".Rmd", sep = ""),
         output = paste(dir, "/", file, ".R", sep = ""))
    cat("\nR code saved in ", dir, "/", file, ".R\n", sep = "")
    
  }
  
  ## open output file (i.e. PDF) if requested
  if(open_output == TRUE)
    system(paste("open ", dir, "/", file, ".pdf", sep = ""))
  
  cat("\n")
}

cat_doc <- function(doc, filename){
  sink(filename)
  for(i in 1:length(doc))
    cat(doc[[i]], "\n\n")
  sink()
}

tex_header <- function(title, level){
  paste(paste(rep("#", level), collapse = ""), title)
} 

title_header <- function(title = NULL, authors = NULL, abstract = NULL, keep_tex = FALSE, pre_register = TRUE){
  
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
               "\noutput:\n  pdf_document:\n    toc: yes",
               ifelse(pre_register, "\n    template: ./egap_registration_template.tex", ""),
               ifelse(keep_tex, "\n    keep_tex: true", ""),
               "\n---\n\n", 
               sep = ""))
  
}

#' @importFrom formatR tidy_source
code_snippet <- function(..., ## takes a character string
                         results = "asis", 
                         echo = FALSE,
                         tidy = TRUE, tidy.opts = "list(width.cutoff = 50)",
                         strip_white = TRUE) {
  return(paste("```{r, echo = ", echo, ", tidy = ", tidy, ", tidy.opts = ", tidy.opts, ", strip_white = ", strip_white, ", warning = FALSE, results = '", results, "'}\n", 
               paste(unlist(list(...)), collapse = ""),
               "\n```", sep = ""))
}

#' @importFrom knitr kable
#' @export
treatment_table <- function(design, caption = "Description of each treatment condition", ...){
  treatment_table <- data.frame(design$condition_names, "")
  colnames(treatment_table) <- c("Treatment condition", "Description")
  kable(treatment_table, caption = caption, row.names = FALSE, ... = ...)
}

#' @importFrom knitr kable
#' @export
units_table <- function(sample_frame, ...){
  units_table <- summary(sample_frame)
  kable(units_table, caption = "Levels of analysis", row.names = FALSE, ... = ...)
}

