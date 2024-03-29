#' Initializes a parameter
#' @param param a parameter,
#' using \code{\link{create_param}}
#' @param id the parameter's ID. Will be ignored if the parameter already
#'   has an ID
#' @return an initialized parameter
#' @author Richèl J.C. Bilderbeek
#' @export
init_param <- function(
  param,
  id
) {
  check_true(is_param(param))

  if (is_one_na(param$id)) {
    param$id <- id
  }

  param
}
