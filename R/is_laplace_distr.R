#' Determine if the object is a valid
#' laplace distribution,
#' as created by \code{\link{create_laplace_distr}}
#' @param x an object, to be determined if it is a valid
#'   laplace distribution
#' @return TRUE if x is a valid laplace distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @examples
#'   laplace_distr <- create_laplace_distr()
#'   testit::assert(beautier:::is_laplace_distr(laplace_distr))
is_laplace_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "laplace") return(FALSE)
  if (!"mu" %in% names(x)) return(FALSE)
  if (!is_mu_param(x$mu)) return(FALSE)
  if (!"scale" %in% names(x)) return(FALSE)
  if (!is_scale_param(x$scale)) return(FALSE)
  TRUE
}
