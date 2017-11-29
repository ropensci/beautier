#' Determine if the object is a valid
#' 'rate CG' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate CG' parameter
#' @return TRUE if x is a valid 'rate CG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_cg_param}} creates a 'rate CG' parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_cg_param <- create_rate_cg_param()
#'   testit::assert(is_rate_cg_param(rate_cg_param))
#' @export
is_rate_cg_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "rate_cg"
}
