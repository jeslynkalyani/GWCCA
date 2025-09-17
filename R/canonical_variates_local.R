#' Compute the Influence of a Certain Canonical Variate in Different Locations
#'
#' @param U1 numeric vector, the first canonical variate from X
#' @param V1 numeric vector, the first canonical variate from Y
#' @param loc_id vector, location ID for each observation
#' @param lw_mat matrix, spatial weights matrix
#' @return list with local U1 and V1 variates
#' @export
cca_variates_local <- function(U1, V1, loc_id, lw_mat) {
  U1_loc <- tapply(U1, loc_id, mean)
  V1_loc <- tapply(V1, loc_id, mean)

  list(
    U1_local = lw_mat %*% as.numeric(U1_loc),
    V1_local = lw_mat %*% as.numeric(V1_loc)
  )
}
