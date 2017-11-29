#' Determine if the object is a valid lambda parameter
#' @param x an object, to be determined if it is a valid
#'   lambda parameter
#' @return TRUE if x is a valid lambda parameter,
#'   FALSE otherwise
#' @seealso lambda parameters are returned by \code{\link{create_lambda_param}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   lambda_param <- create_lambda_param()
#'   testit::assert(is_lambda_param(lambda_param))
#' @export
is_lambda_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "lambda"
}
