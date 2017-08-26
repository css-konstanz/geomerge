# EXTRACT first and second order neighbors (polygons)
# - add option and implement row and column standardized weights here!
geomerge.neighbor <- function(polygon_input) {
  nghbrs <- poly2nb(polygon_input)
  # nblag generates higher order neighbors (see p249 Bivand book)
  nb_lags <- nblag(nghbrs, maxlag=2)
  # default is row-standardized weights
  wts1 <- nb2listw(nb_lags[[1]],zero.policy = TRUE)  # islands should be cut, so no need for zero.policy=TRUE
  wts2 <- nb2listw(nb_lags[[2]],zero.policy = TRUE)
  list(wts1=wts1, wts2=wts2)
}