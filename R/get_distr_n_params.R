#' Get the number of parameters a distribution uses
#' @param distr a distribution,
#'   as created by \code{\link{create_distr}} or (preferable)
#'   its named functions
#' @return the number of parameters that distribution uses
#' @author Richel J.C. Bilderbeek
#' @export
get_distr_n_params <- function(
  distr
) {
  if (!beautier::is_distr(distr)) {
    stop("'distr' must be a distribution")
  }

  if (beautier::is_beta_distr(distr)) {
    return(2) # alpha and beta
  } else if (beautier::is_exponential_distr(distr)) {
    return(1) # mean
  } else if (beautier::is_gamma_distr(distr)) {
    return(2) # alpha and beta
  } else if (beautier::is_inv_gamma_distr(distr)) {
    return(2) # alpha and beta
  } else if (beautier::is_laplace_distr(distr)) {
    return(2) # mu and scale
  } else if (beautier::is_log_normal_distr(distr)) {
    return(2) # m and s
  } else if (beautier::is_normal_distr(distr)) {
    return(2) # mean and sigma
  } else if (beautier::is_one_div_x_distr(distr)) {
    return(0) # none
  } else  if (beautier::is_poisson_distr(distr)) {
    return(1) # lambda
  } else {
    testit::assert(beautier::is_uniform_distr(distr))
    return(0) # none
  }
}
