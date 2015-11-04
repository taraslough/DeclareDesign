fx_names <- ls("package:DeclareDesign")

arg_names <- sapply(X = fx_names, FUN = formals)

names(arg_names[[1]])

all_argument_names <- as.character(unlist(sapply(arg_names, function(x) names(x))))

unique_argument_names <- sort(unique(all_argument_names))



