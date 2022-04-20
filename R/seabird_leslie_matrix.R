get_seabird_matrix <- function(fertility, survival, correction) {
  matrix_leslie <- matrix_leslie(fertility, survival)
  nrow <- length(fertility)
  matrix_leslie[nrow, nrow] <- correction
  return(matrix_leslie)
}
