#' Determine if the object is a valid
#' 'rate CT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate CT' parameter
#' @return TRUE if x is a valid 'rate CG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ct_param}} creates a 'rate CT' parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_ct_param <- create_rate_ct_param()
#'   testit::assert(is_rate_ct_param(rate_ct_param))
#' @export
is_rate_ct_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "rate_ct"
}
