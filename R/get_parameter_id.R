#' Get the parameter's ID
#' @param parameter a distibution,
#'   as created by \code{\link{create_parameter}})
#' @return the parameter's ID
#' @author Richel J.C. Bilderbeek
#' @export
get_parameter_id <- function(parameter) {
  if (!is_parameter(parameter)) {
    stop("Must supply a valid parameter")
  }
  testit::assert("id" %in% names(parameter))
  return(parameter$id)
}
