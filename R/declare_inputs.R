
#' @export
declare_inputs <- function(data_list = NULL, data_file = NULL, code_file = NULL){
  
  input_environment <- new.env()
  
  if(!class(data_file) == "list"){
    data_file <- list(data_file)
  }
  
  if(!is.null(data_file)){
    for(j in 1:length(data_file)){
      load(data_file[[j]], envir = input_environment)
    }
  }
  
  data <- as.list(input_environment)
  
  if(!is.null(data_list)){
    if(any(names(data_list) %in% ls(input_environment)))
      stop("Please do not include objects in data_list that are also stored in data_file.")
    
    data <- c(data_list, data)
  }
  
  for(i in 1:length(data)){
    if(class(data[[i]]) == "function")
      warning(paste0("The object ", names(data)[[i]], "that you included in data_list or data_file is a function. To improve the transparency of your design declaration, we encourage you to include functions in the code file."))
  }
  
  code <- readLines(con = code_file)
  
  check_code(code)
  
  required_packages <- get_package_dependencies(code)
  
  versions <- get_versions(packages = required_packages)
  
  return_list <- list(data = data, code = code, required_packages = required_packages, versions = versions, call = match.call())
  
  class(return_list) <- "inputs"
  
  return(return_list)
  
}

get_versions <- function(packages = NULL){
  installed <- installed.packages()[, c("Package", "Version")]
  
  if(is.null(packages)){
    packages <- loadedNamespaces()
  }
  if(!("DeclareDesign" %in% packages)){
    packages <- c("DeclareDesign", packages)
  }
  
  packages <- installed[installed[,1] %in% packages  &
                        !(installed[,1] %in% c("base", "datasets", "graphics", 
                                               "grDevices", "methods", "stats", "tools", "utils")), ]
  DeclareDesign_ver <- packages[packages[,1] %in% "DeclareDesign"][2]
  packages <- packages[!(packages[,1] %in% "DeclareDesign"), ]
  
  if (.Platform$OS.type == "windows") {
    running <- win.version()
  } else if (nzchar(Sys.which("uname"))) {
    uname <- system("uname -a", intern = TRUE)
    os <- sub(" .*", "", uname)
    running <- switch(os, Linux = if (file.exists("/etc/os-release")) {
      tmp <- readLines("/etc/os-release")
      t2 <- if (any(grepl("^PRETTY_NAME=", tmp))) sub("^PRETTY_NAME=", 
                                                      "", grep("^PRETTY_NAME=", tmp, value = TRUE)[1L]) else if (any(grepl("^NAME", 
                                                                                                                           tmp))) sub("^NAME=", "", grep("^NAME=", tmp, 
                                                                                                                                                         value = TRUE)[1L]) else "Linux (unknown distro)"
      sub("\"(.*)\"", "\\1", t2)
    } else if (file.exists("/etc/system-release")) {
      readLines("/etc/system-release")
    }, Darwin = {
      ver <- readLines("/System/Library/CoreServices/SystemVersion.plist")
      ind <- grep("ProductUserVisibleVersion", ver)
      ver <- ver[ind + 1L]
      ver <- sub(".*<string>", "", ver)
      ver <- sub("</string>$", "", ver)
      ver1 <- strsplit(ver, ".", fixed = TRUE)[[1L]][2L]
      sprintf("OS X %s (%s)", ver, switch(ver1, `6` = "Snow Leopard", 
                                          `7` = "Lion", `8` = "Mountain Lion", `9` = "Mavericks", 
                                          `10` = "Yosemite", `11` = "El Capitan", "unknown"))
    }, SunOS = {
      ver <- system("uname -r", intern = TRUE)
      paste("Solaris", strsplit(ver, ".", fixed = TRUE)[[1L]][2L])
    }, uname)
  }
  
  return(packages = packages, DeclareDesign = DeclareDesign_ver, R = R.version.string, OS = running)
  
}

check_code <- function(code){
  
  prohibited_list <- c("source", "load", "read.csv", "read.csv2", "read.delim", "read.delim2", "read.table", "read.xlsx", "read.xls", "readxl", "readShapePoly", "readShapeLines", "readShapePoints")
 
  prohibited_used <- c()
  for(i in 1:length(prohibited_list)){
    if(any(grepl(paste0('\\b', prohibited_list[i], '\\('), code))){
      prohibited_used <- c(prohibited_used, prohibited_list[i])
    }
  }
  
  if(length(prohibited_used) > 0){
    warning(paste0("Your code file includes expressions that may make replication in the future or on other computers difficult or impossible. These commands include: ", paste(substr(prohibited_used, 1, nchar(prohibited_used)), collapse = ", "), ". These commands load external resources from files on your computer. Consider including all necessary files in the data_file or the data_list, and dropping these commands from your code."))
  }
   
}

#' @export
save_design_replication_files <- function(design){
  
  save(list = names(design$inputs$data), envir = list2env(design$inputs$data), file = "design_data_inputs.RData")
  writeLines(text = design$inputs$code, con = "design_code_inputs.R")
  
  design_declaration <- c("## Design declaration written by the R package DeclareDesign", 
                          "library(devtools)",
                          "install_github(\"DeclareDesign/DeclareDesign\", ref = \"", DeclareDesign_ref, "\")",
                          paste0("## ", print_versions(versions = design$inputs$versions)),
                          timestamp(), 
                          "")
  if(!is.null(design$inputs$code)){ 
    design_declaration <- c(design_declaration, "## Run user-written code to create inputs to the design declaration", "source('design_code_inputs.R')", "")
  }
  if(!is.null(design$inputs$data)){
    design_declaration <- c(design_declaration, "## Load user-created data as inputs to the design declaration", "load('design_data_inputs.RData')", "")
  }
  
  design_declaration <- c(design_declaration, summarize_code(design))
  
  writeLines(text = design_declaration, con = "design_declaration.R")
  
}

get_package_dependencies <- function(code) {
  ## was fileDependencies.R

  # build a list of package dependencies to return
  pkgs <- character()
  
  # parse file and examine expressions
  tryCatch({
    exprs <- suppressWarnings(parse(text = code))
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  }, error = function(e) {
    warning(paste("Failed to parse your code file; dependencies in this file will not be discovered."))
  })
  
  # return packages
  unique(pkgs)
}

anyOf <- function(object, ...) {
  predicates <- list(...)
  for (predicate in predicates)
    if (predicate(object))
      return(TRUE)
  FALSE
}

allOf <- function(object, ...) {
  predicates <- list(...)
  for (predicate in predicates)
    if (!predicate(object))
      return(FALSE)
  TRUE
}

recursiveWalk <- function(`_node`, fn, ...) {
  fn(`_node`, ...)
  if (is.call(`_node`)) {
    for (i in seq_along(`_node`)) {
      recursiveWalk(`_node`[[i]], fn, ...)
    }
  }
}

# Fills 'env' as a side effect
identifyPackagesUsed <- function(call, env) {
  
  if (!is.call(call))
    return()
  
  fn <- call[[1]]
  if (!anyOf(fn, is.character, is.symbol))
    return()
  
  fnString <- as.character(fn)
  
  # Check for '::', ':::'
  if (fnString %in% c("::", ":::")) {
    if (anyOf(call[[2]], is.character, is.symbol)) {
      pkg <- as.character(call[[2]])
      env[[pkg]] <- TRUE
      return()
    }
  }
  
  # Check for S4-related function calls (implying a dependency on methods)
  if (fnString %in% c("setClass", "setMethod", "setRefClass", "setGeneric", "setGroupGeneric")) {
    env[["methods"]] <- TRUE
    return()
  }
  
  # Check for packge loaders
  pkgLoaders <- c("library", "require", "loadNamespace", "requireNamespace")
  if (!fnString %in% pkgLoaders)
    return()
  
  # Try matching the call.
  loader <- tryCatch(
    get(fnString, envir = asNamespace("base")),
    error = function(e) NULL
  )
  
  if (!is.function(loader))
    return()
  
  matched <- match.call(loader, call)
  if (!"package" %in% names(matched))
    return()
  
  # Protect against 'character.only = TRUE' + symbols.
  # This defends us against a construct like:
  #
  #    for (x in pkgs)
  #        library(x, character.only = TRUE)
  #
  if ("character.only" %in% names(matched)) {
    if (is.symbol(matched[["package"]])) {
      return()
    }
  }
  
  if (anyOf(matched[["package"]], is.symbol, is.character)) {
    pkg <- as.character(matched[["package"]])
    env[[pkg]] <- TRUE
    return()
  }
  
  
}

expressionDependencies <- function(e) {
  
  if (is.expression(e)) {
    return(unlist(lapply(e, function(call) {
      expressionDependencies(call)
    })))
  }
  
  else if (is.call(e)) {
    env <- new.env(parent = emptyenv())
    recursiveWalk(e, identifyPackagesUsed, env)
    return(ls(env, all.names = TRUE))
  }
  
  else character()
  
}


