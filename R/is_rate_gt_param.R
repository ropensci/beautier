#' Determine if the object is a valid
#' 'rate GT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate GT' parameter
#' @return TRUE if x is a valid 'rate GT' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_gt_param}} creates a 'rate GT' parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_gt_param <- create_rate_gt_param()
#'   testit::assert(beautier:::is_rate_gt_param(rate_gt_param))
is_rate_gt_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_gt"
}
