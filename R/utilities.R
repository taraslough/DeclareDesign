integerize <- function(data){
  for(i in 1:ncol(data)){
    numeric_check <- FALSE
    numeric_check <- class(data[,i]) %in% c("numeric","integer")
    
    if(!numeric_check){
      suppressWarnings(numeric_check <- identical(data[,i], as.factor(as.integer(as.character(data[,i])))))
      if(!numeric_check){
        suppressWarnings(numeric_check <- identical(data[,i], as.factor(as.numeric(as.character(data[,i])))))
        if(!numeric_check){
          suppressWarnings(numeric_check <- identical(data[,i], as.numeric(as.character(data[,i]))))
          if(!numeric_check){
            suppressWarnings(numeric_check <- identical(data[,i], as.numeric(as.character(data[,i]))))
          }
        }
      }
      if(numeric_check){
        data[,i] <- as.integer(as.character(data[,i]))
      }
    }
  }
  return(data)
}

trim_spaces <- function(text){
  gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", text))
}

#' Print version of R and packages to improve reproducibility
#'
#' @export
print_versions <- function(){
  
  installed <- installed.packages()[, c("Package", "Version")]
  loaded <- installed[installed[,1] %in% loadedNamespaces() &
                        !(installed[,1] %in% c("base", "datasets", "graphics", 
                                               "grDevices", "methods", "stats", "tools", "utils")), ]
  DeclareDesign_ver <- loaded[loaded[,1] %in% "DeclareDesign"][2]
  loaded <- loaded[!(loaded[,1] %in% "DeclareDesign"), ]
  
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
  
  cat("This document was compiled with the R package DeclareDesign ", ifelse(!is.na(DeclareDesign_ver), paste0("version ", DeclareDesign_ver, " "), ""), 
      "using ", R.version.string, " with the ", running, " operating system and the R packages: ", sep = "")
  
  loaded[, 2] <- paste0("(", loaded[, 2], ")")
  loaded <- apply(loaded, 1, function(x) paste(x, collapse = " "))
  
  if(length(loaded) == 1)
    cat(loaded)
  if(length(loaded) == 2)
    cat(paste(loaded, collapse = " and "))
  else
    cat(paste(loaded[1:(length(loaded)-1)], collapse = "; "), "; and ", loaded[length(loaded)], sep = "")
  
  cat(".")
  
}

reorient <- function(x) {
  obj <- c(x)
  names(obj) <- rep(paste(rownames(x), colnames(x), sep = "_"), each = ncol(x))
  return(obj)
}
