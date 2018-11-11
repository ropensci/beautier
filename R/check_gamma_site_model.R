#' Checks if the parameter is a valid gamma site model
#' @param x the object to be determined if it is a valid gamma site object
#' @return nothing. Will call \code{stop} if the argument is not a valid
#'   gamma site model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_silent(
#'     beautier:::check_gamma_site_model(
#'       create_gamma_site_model()
#'     )
#'   )
#'   testthat::expect_error(
#'     beautier:::check_gamma_site_model(
#'       "not a gamma site model"
#'     )
#'   )
#' @noRd
check_gamma_site_model <- function(gamma_site_model) {
  if (!is_gamma_site_model(gamma_site_model)) { # nolint internal function
    stop("'gamma_site_model' must be a valid gamma site model")
  }
}
