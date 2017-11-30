#' Get the parameter's ID
#' @param parameter a parameter,
#'   as created by \code{\link{create_param}})
#' @return the parameter's ID
#' @author Richel J.C. Bilderbeek
#' @export
get_param_id <- function(parameter) {
  if (!beautier::is_param(parameter)) {
    stop("Must supply a valid parameter")
  }
  testit::assert("id" %in% names(parameter))
  parameter$id
}
