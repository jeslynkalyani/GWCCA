#' Moran's I Test for Spatial Dependence in Canonical Variates
#'
#' @param U1 numeric vector, the first canonical variate from X
#' @param V1 numeric vector, the first canonical variate from Y
#' @param loc_id vector, location ID for each observation
#' @param lw listw object, spatial weights
#' @return list of Moran's I test results for U1 and V1
#' @export
cca_moran <- function(U1, V1, loc_id, lw) {
  U1_loc <- tapply(U1, loc_id, mean)
  V1_loc <- tapply(V1, loc_id, mean)

  moran_u1 <- spdep::moran.test(as.numeric(U1_loc), lw)
  moran_v1 <- spdep::moran.test(as.numeric(V1_loc), lw)

  list(
    moran_u1 = moran_u1,
    moran_v1 = moran_v1
  )
}
