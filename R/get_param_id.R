#' Get the parameter's ID
#' @param parameter a distibution,
#'   as created by \code{\link{createparam}})
#' @return the parameter's ID
#' @author Richel J.C. Bilderbeek
#' @export
getparam_id <- function(parameter) {
  if (!isparam(parameter)) {
    stop("Must supply a valid parameter")
  }
  testit::assert("id" %in% names(parameter))
  return(parameter$id)
}
