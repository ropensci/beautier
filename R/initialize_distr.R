#' Initializes a distribution
#' @param distribution a distribution,
#' using \code{\link{create_distribution}}
#' @param distr_id the first distribution's ID
#' @param param_id the first parameter's ID
#' @return an initialized distribution
#' @author Richel J.C. Bilderbeek
initialize_distr <- function(
  distribution,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_distribution(distribution))

  if (is.na(distribution$id)) {
    distribution$id <- distr_id
  }

  if (is_beta_distribution(distribution)) {

    if (is.na(distribution$alpha$id)) {
      distribution$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is.na(distribution$beta$id)) {
      distribution$beta$id <- param_id
    }

  } else if (is_exponential_distribution(distribution)) {

    # TODO

  } else if (is_gamma_distribution(distribution)) {

    if (is.na(distribution$alpha$id)) {
      distribution$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is.na(distribution$beta$id)) {
      distribution$beta$id <- param_id
    }

  } else if (is_inv_gamma_distribution(distribution)) {

    # TODO

  } else if (is_laplace_distribution(distribution)) {

    if (is.na(distribution$mu$id)) {
      distribution$mu$id <- param_id
      param_id <- param_id + 1
    }
    if (is.na(distribution$scale$id)) {
      distribution$scale$id <- param_id
    }

  } else if (is_log_normal_distribution(distribution)) {

    # TODO

  } else if (is_normal_distribution(distribution)) {

    # TODO

  } else if (is_one_div_x_distribution(distribution)) {

    # TODO

  } else  if (is_poisson_distribution(distribution)) {

    # TODO

  } else {
    testit::assert(is_uniform_distribution(distribution))

    # TODO

  }
  distribution
}
