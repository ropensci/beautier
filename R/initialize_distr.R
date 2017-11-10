#' Initializes a distribution
#' @param distribution a distribution,
#' using \code{\link{create_distr}}
#' @param distr_id the first distribution's ID
#' @param param_id the first parameter's ID
#' @return an initialized distribution
#' @author Richel J.C. Bilderbeek
initialize_distr <- function(
  distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_distr(distr))

  if (is.na(distr$id)) {
    distr$id <- distr_id
  }

  if (is_beta_distr(distr)) {

    if (is.na(distr$alpha$id)) {
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is.na(distr$beta$id)) {
      distr$beta$id <- param_id
    }

  } else if (is_exponential_distr(distr)) {

    # TODO

  } else if (is_gamma_distr(distr)) {

    if (is.na(distr$alpha$id)) {
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is.na(distr$beta$id)) {
      distr$beta$id <- param_id
    }

  } else if (is_inv_gamma_distr(distr)) {

    # TODO

  } else if (is_laplace_distr(distr)) {

    if (is.na(distr$mu$id)) {
      distr$mu$id <- param_id
      param_id <- param_id + 1
    }
    if (is.na(distr$scale$id)) {
      distr$scale$id <- param_id
    }

  } else if (is_log_normal_distr(distr)) {

    # TODO

  } else if (is_normal_distr(distr)) {

    # TODO

  } else if (is_one_div_x_distr(distr)) {

    # TODO

  } else  if (is_poisson_distr(distr)) {

    # TODO

  } else {
    testit::assert(is_uniform_distr(distr))

    # TODO

  }
  distr
}
