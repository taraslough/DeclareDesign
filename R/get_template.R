#' @export
get_template <- function(template_name){
  DD_template_db <- "http://www.jasper-cooper.com/dd_test/"
  template_url <- paste0(DD_template_db,template_name,".R")
  source(template_url)
}
