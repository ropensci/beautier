#' Get the number of parameters a distribution uses
#' @param distr a distribution,
#'   as created by \code{\link{create_distr}} or (preferable)
#'   its named functions
#' @return the number of parameters that distribution uses
#' @examples
#' library(testthat)
#' expect_equal(get_distr_n_params(create_beta_distr()), 2)
#' expect_equal(get_distr_n_params(create_exp_distr()), 1)
#' expect_equal(get_distr_n_params(create_gamma_distr()), 2)
#' expect_equal(get_distr_n_params(create_inv_gamma_distr()), 2)
#' expect_equal(get_distr_n_params(create_laplace_distr()), 2)
#' expect_equal(get_distr_n_params(create_log_normal_distr()), 2)
#' expect_equal(get_distr_n_params(create_normal_distr()), 2)
#' expect_equal(get_distr_n_params(create_one_div_x_distr()), 0)
#' expect_equal(get_distr_n_params(create_poisson_distr()), 1)
#' expect_equal(get_distr_n_params(create_uniform_distr()), 0)
#' @author Richèl J.C. Bilderbeek
#' @export
get_distr_n_params <- function(
  distr
) {
  if (!beautier::is_distr(distr)) {
    stop("'distr' must be a distribution")
  }

  if (beautier::is_beta_distr(distr)) {
    return(2) # alpha and beta
  } else if (beautier::is_exp_distr(distr)) {
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
