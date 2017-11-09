#' Get the number of parameters a distribution uses
#' @param distribution a distribution,
#'   as created by \code{\link{create_distribution}} or (preferable)
#'   its named functions
#' @return the number of parameters that distribution uses
#' @author Richel J.C. Bilderbeek
#' @export
get_distr_n_params <- function(
  distribution
) {
  if (!is_distribution(distribution)) {
    stop("Must supply a distribution")
  }

  if (is_beta_distribution(distribution)) {
    return(2) # alpha and beta
  } else if (is_exponential_distribution(distribution)) {
    return(NA)
  } else if (is_gamma_distribution(distribution)) {
    return(2) # alpha and beta
  } else if (is_inv_gamma_distribution(distribution)) {
    return(NA)
  } else if (is_laplace_distribution(distribution)) {
    return(2) # mu and scale
  } else if (is_log_normal_distribution(distribution)) {
    return(NA)
  } else if (is_normal_distribution(distribution)) {
    return(NA)
  } else if (is_one_div_x_distribution(distribution)) {
    return(NA)
  } else  if (is_poisson_distribution(distribution)) {
    return(NA)
  } else {
    testit::assert(is_uniform_distribution(distribution))
    return(NA)
  }
}
