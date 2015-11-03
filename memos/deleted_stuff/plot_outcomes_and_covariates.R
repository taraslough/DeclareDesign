#' Plot the frequency or density of potential outcomes 
#'
#' @param data The dataframe that contains the outcomes to plot (i.e. made using \code{\link{draw_population}} or \code{\link{draw_sample}})
#' @param outcome_names A vector of names of the outcomes to plot
#' @param color_palette Optional vector of custom colors for the plot
#' @param barplot If TRUE, plot is a barplot
#' @param density If TRUE, plot is a density plot
#' @param histogram If TRUE, plot is a frequency histogram
#' @param ... Optional graphical arguments
#' @export
plot_outcomes <- function(data,outcome_names,color_palette = NULL,barplot = FALSE,density = FALSE,histogram = FALSE,...){
  
  if(!all(outcome_names %in% colnames(data))){
    stop("The outcome_names vector must match the names of the outcomes in the data object.")
  }
  
  if(sum(c(barplot,density,histogram)*1)>1){
    stop("You may only specify one graphical output at a time.")
  }
  
  if(is.null(color_palette)){
    color_palette <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', 
                       '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', 
                       '#D9D9D9', '#BC80BD', '#CCEBC5', '#FFED6F', 
                       "#C977D9", "#A18AE6", "#8AA2E6", "#8BD1E7", "#8AF3CF", 
                       "#85F38E", "#BDF385", "#EDE485", "#F0B086", "#DE9F8B", 
                       "#74A3B3", "#99CC70", "#DCD68E", "#EDDFAD", "#F7E8CA", 
                       "#FFF9F3", "#FFF9F6", "#FFFBF9", "#FFFCFA", "#FFFEFD")
  }
  
  columns <- subset(data,select = outcome_names)
  unique_outcomes <- unique(unlist(columns))
  N_uniq_out <- length(unique_outcomes)
  
  make_barplot <- function(columns,outcome_names,color_palette,...){
    bar_mat <- apply(columns,2,function(col){
      col<- factor(x = col,levels = unique_outcomes)
      table(col)}
    )
    outcomes <- rownames(bar_mat)
    barplot(height = bar_mat,col = color_palette[1:length(outcomes)],beside = T,legend = outcomes,...)
  }
  
  make_density <- function(columns,outcome_names,color_palette,...){
    plot(density(columns[,1]),col = color_palette[1],...)
    if(ncol(columns)>1){
      for (i in 2:ncol(columns)){
        lines(density(columns[,i]),col = color_palette[i])
      }
    }
    legend(x = "topright",y = colnames(columns),lty = rep(1,ncol(columns)),col = color_palette[1:ncol(columns)],...)
  }
  
  make_histogram <- function(columns,outcome_names,color_palette,...){
    hist(columns[,1],col = color_palette[1],...)
    if(ncol(columns)>1){
      for (i in 2:ncol(columns)){
        hist(columns[,i],col = color_palette[i],add = T,...)
      }
    }
    legend(x = "topright",y = colnames(columns),fill = color_palette[1:ncol(columns)])
  }
  
  
  if(barplot){
    return(make_barplot(columns = columns,outcome_names = outcome_names,color_palette = color_palette,... = ...))
  }
  
  if(density){
    return(make_density(columns = columns,outcome_names = outcome_names,color_palette = color_palette,... = ...))
  }
  
  if(histogram){
    return(make_histogram(columns = columns,outcome_names = outcome_names,color_palette = color_palette,... = ...))
  }
  
  if(N_uniq_out>50){
    return(make_density(columns = columns,outcome_names = outcome_names,color_palette = color_palette,... = ...))
  }
  
  if(N_uniq_out>5){
    return(make_histogram(columns = columns,outcome_names = outcome_names,color_palette = color_palette,... = ...))
  }
  
  return(make_barplot(columns = columns,outcome_names = outcome_names,color_palette = color_palette,... = ...))
  
}


#' Plot the frequency or density of covariates
#'
#' @param data The dataframe that contains the outcomes to plot (i.e. made using \code{\link{draw_population}})
#' @param covariate_names A vector of names of the outcomes to plot
#' @param color_palette Optional vector of custom colors for the plot
#' @param barplot If TRUE, plot is a barplot
#' @param density If TRUE, plot is a density plot
#' @param histogram If TRUE, plot is a frequency histogram
#' @param ... Optional graphical arguments
#' @export
plot_covariates <- function(data,covariate_names,color_palette = NULL,barplot = FALSE,density = FALSE,histogram = FALSE,...){
  
  if(!all(covariate_names %in% colnames(data))){
    stop("The outcome_names vector must match the names of the covariates in the data object.")
  }
  
  if(sum(c(barplot,density,histogram)*1)>1){
    stop("You may only specify one graphical output at a time.")
  }
  
  if(is.null(color_palette)){
    color_palette <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', 
                       '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', 
                       '#D9D9D9', '#BC80BD', '#CCEBC5', '#FFED6F', 
                       "#C977D9", "#A18AE6", "#8AA2E6", "#8BD1E7", "#8AF3CF", 
                       "#85F38E", "#BDF385", "#EDE485", "#F0B086", "#DE9F8B", 
                       "#74A3B3", "#99CC70", "#DCD68E", "#EDDFAD", "#F7E8CA", 
                       "#FFF9F3", "#FFF9F6", "#FFFBF9", "#FFFCFA", "#FFFEFD")
  }
  
  columns <- subset(data,select = covariate_names)
  unique_covariates <- unique(unlist(columns))
  N_uniq_out <- length(unique_covariates)
  
  make_barplot <- function(columns,covariate_names,color_palette,...){
    bar_mat <- apply(columns,2,function(col){
      col<- factor(x = col,levels = unique_covariates)
      table(col)}
    )
    covariates <- rownames(bar_mat)
    barplot(height = bar_mat,col = color_palette[1:length(covariates)],beside = T,legend = covariates,...)
  }
  
  make_density <- function(columns,covariate_names,color_palette,...){
    plot(density(columns[,1]),col = color_palette[1],...)
    if(ncol(columns)>1){
      for (i in 2:ncol(columns)){
        lines(density(columns[,i]),col = color_palette[i])
      }
    }
    legend(x = "topright",y = colnames(columns),lty = rep(1,ncol(columns)),col = color_palette[1:ncol(columns)],...)
  }
  
  make_histogram <- function(columns,covariate_names,color_palette,...){
    hist(columns[,1],col = color_palette[1],...)
    if(ncol(columns)>1){
      for (i in 2:ncol(columns)){
        hist(columns[,i],col = color_palette[i],add = T,...)
      }
    }
    legend(x = "topright",y = colnames(columns),fill = color_palette[1:ncol(columns)])
  }
  
  
  if(barplot){
    return(make_barplot(columns = columns,covariate_names = covariate_names,color_palette = color_palette,... = ...))
  }
  
  if(density){
    return(make_density(columns = columns,covariate_names = covariate_names,color_palette = color_palette,... = ...))
  }
  
  if(histogram){
    return(make_histogram(columns = columns,covariate_names = covariate_names,color_palette = color_palette,... = ...))
  }
  
  if(N_uniq_out>50){
    return(make_density(columns = columns,covariate_names = covariate_names,color_palette = color_palette,... = ...))
  }
  
  if(N_uniq_out>5){
    return(make_histogram(columns = columns,covariate_names = covariate_names,color_palette = color_palette,... = ...))
  }
  
  return(make_barplot(columns = columns,covariate_names = covariate_names,color_palette = color_palette,... = ...))
  
}
