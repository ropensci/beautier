#' Determine if the object is a valid
#' lambda parameter
#' @param x an object, to be determined if it is a valid
#'   lambda parameter
#' @return TRUE if x is a valid lambda parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_lambda_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "lambda")
}
