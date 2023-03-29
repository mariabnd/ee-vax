#' Plot time varying transmission matrices
#' 
#' @description Given an array of time varying transmission weight matrices,
#' they are plotted as heatmaps using the dimension names given for the array
#' and the range of the entire array to ensure standardised limits in the plots
#' @param mat_list An array of transmission weights
#' @labs Labels to use on x and y axis as well as 
#' @expanded Boolean determining whether the matrices are both units or single
#' units. Labels are skipped (using `skip_label()`) if true
#' @adj_size Factor by which to scale axis labels by, defaults to 0
plot_matrices <- function(mat_list, labs, expanded, adj_size = 0){
  if(class(mat_list) != "array"){
    stop("mat_list must be an array")
  }
  if(length(dim(mat_list)) != 3){
    stop("mat_list must have dimension 3")
  }
  if(length(labs) != 3){
    stop("labs must have length 3")
  }
  if(!is.logical(expanded)){
    stop("expanded must be TRUE/FALSE")
  }
  out <- lapply(seq_len(dim(mat_list)[3]), FUN = function(mat) {
    tmp <- melt(mat_list[, , mat])
    p <- ggplot(data = tmp, aes_string(x = names(tmp)[1], y = names(tmp)[2],
                                  fill = names(tmp)[3])) + 
      geom_tile() +
      labs(x = labs[1], y = labs[2],
           title = dimnames(mat_list)[[3]][mat],
           fill = labs[3]) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.text = element_text(size = theme_get()$axis.text$size + adj_size)) +
      scale_fill_viridis_c(direction = - 1,
                           limits = range(floor(mat_list),
                                          ceiling(mat_list))) +
      coord_fixed()
    if(isTRUE(expanded)){
    p <- p +
      # Skip some of the many labels if using the product
      scale_y_discrete(labels = skip_labels(tmp[, 2])) +
      scale_x_discrete(labels = skip_labels(tmp[, 1]))
    }
    return(p)
  })
  return(out)
}