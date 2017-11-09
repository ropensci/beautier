#' Determine if the object is a valid distribution
#' @param x an object, to be determined if it is a valid
#'   distribution
#' @return TRUE if x is a valid distribution,
#'   FALSE otherwise
#' @seealso use
#'  \code{\link{is_beta_distribution}},
#'  \code{\link{is_exponential_distribution}},
#'  \code{\link{is_gamma_distribution}},
#'  \code{\link{is_inv_gamma_distribution}},
#'  \code{\link{is_laplace_distribution}},
#'  \code{\link{is_log_normal_distribution}},
#'  \code{\link{is_normal_distribution}},
#'  \code{\link{is_one_div_x_distribution}},
#'  \code{\link{is_poisson_distribution}},
#'  or \code{\link{is_uniform_distribution}},
#'  to check for more specific distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_distribution <- function(
  x
) {
  return("name" %in% names(x))
}
