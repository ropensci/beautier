#' Determine if x is an initialized clock_model object,
#' as created by \code{\link{create_clock_model}}
#' @param x the object to check if it is an
#'   initialized clock_models object
#' @return TRUE if x is an initialized clock_model object
#' @author Richel J.C. Bilderbeek
is_initialized_clock_model <- function(
  x
) {
  if (!beautier::is_clock_model(x)) return(FALSE)
  if (beautier::is_strict_clock_model(x)) {
    return(is_initialized_strict_clock_model(x)) # nolint internal function call
  } else {
    testit::assert(beautier::is_rln_clock_model(x))
    return(is_initialized_rln_clock_model(x)) # nolint internal function call
  }
}

#' Determine if x is an initialized relaxed log-normal clock_model object,
#' as created by \code{\link{create_rln_clock_model}}
#' @param x the object to check if it is an
#'   initialized relaxed log-normal clock_model object
#' @return TRUE if x is an initialized relaxed log-normal clock_model object
#' @author Richel J.C. Bilderbeek
is_initialized_rln_clock_model <- function(
  x
) {
  testit::assert(beautier::is_rln_clock_model(x))
  is_initialized_distr(x$uclstdev_distr)
}

#' Determine if x is an initialized strict clock_model object
#' as created by \code{\link{create_strict_clock_model}}
#' @param x the object to check if it is an
#'   initialized strict clock model object
#' @return TRUE if x is an initializedstrict clock_model object
#' @author Richel J.C. Bilderbeek
#' @export
is_initialized_strict_clock_model <- function(
  x
) {
  testit::assert(beautier::is_strict_clock_model(x))

  !is.na(x$clock_rate_parameter$id)
}
