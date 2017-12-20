#' Determine if the object is a valid distribution
#' @param x an object, to be determined if it is a valid
#'   distribution
#' @return TRUE if x is a valid distribution,
#'   FALSE otherwise
#' @seealso use
#'  \code{\link{is_beta_distr}},
#'  \code{\link{is_exp_distr}},
#'  \code{\link{is_gamma_distr}},
#'  \code{\link{is_inv_gamma_distr}},
#'  \code{\link{is_laplace_distr}},
#'  \code{\link{is_log_normal_distr}},
#'  \code{\link{is_normal_distr}},
#'  \code{\link{is_one_div_x_distr}},
#'  \code{\link{is_poisson_distr}},
#'  or \code{\link{is_uniform_distr}},
#'  to check for more specific distribution
#' @author Richel J.C. Bilderbeek
is_distr <- function(
  x
) {
  "name" %in% names(x)
}
