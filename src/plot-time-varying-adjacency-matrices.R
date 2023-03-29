if(isFALSE(expanded)) {
  mat_list <- movement_mats
  labs <- c("Canton", "Canton", "Adjacency")
}  
if(isTRUE(expanded)) {
  mat_list <- movement_mats_expanded
  labs <- c("Unit", "Unit", "Transmission weights")
}
out <- plot_matrices(mat_list, labs, expanded, adj_size = - 0.1)
rm(mat_list); rm(labs)