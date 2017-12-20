#' Determine if the object is a valid
#' beta distribution,
#' as created by \code{\link{create_beta_distr}}
#' @param x an object, to be determined if it is a valid
#'   beta distribution,
#' @return TRUE if x is a valid beta distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_beta_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "beta") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!"beta" %in% names(x)) return(FALSE)
  TRUE
}
