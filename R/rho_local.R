#' Compute the Influence of Neighbors Towards the Global CCA Coefficient
#'
#' @param global_cor numeric, the i-th canonical correlation from global (regular) CCA
#' @param lw_mat matrix, spatial weights matrix
#' @return matrix of local canonical correlations
#' @export
rho_local <- function(global_cor, lw_mat) {
  global_cor * lw_mat
}
