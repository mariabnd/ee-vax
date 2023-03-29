if(isFALSE(expanded)) {
  mat_list <- contact_mats
  labs <- c("Age group", "Age group", "Contact")
}
if(isTRUE(expanded)) {
  mat_list <- contact_mats_expanded
  labs <- c("Unit", "Unit", "Transmission weights")
}
out <- plot_matrices(mat_list, labs, expanded)
rm(mat_list); rm(labs)