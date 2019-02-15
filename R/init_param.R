#' Initializes a parameter
#' @param param a parameter,
#' using \code{\link{create_param}}
#' @param id the parameter's ID. Will be ignored if the parameter already
#'   has an ID
#' @return an initialized parameter
#' @author Richèl J.C. Bilderbeek
#' @noRd
init_param <- function(
  param,
  id
) {
  testit::assert(is_param(param)) # nolint beautier function

  if (is.na(param$id)) {
    param$id <- id
  }

  param
}
