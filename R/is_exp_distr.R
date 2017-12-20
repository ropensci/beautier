#' Determine if the object is a valid
#' exponential distribution
#' as created by \code{\link{create_exp_distr}}
#' @param x an object, to be determined if it is a valid
#'   exponential distribution
#' @return TRUE if x is a valid exponential distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_exp_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "exponential") return(FALSE)
  if (!"mean" %in% names(x)) return(FALSE)
  TRUE
}
