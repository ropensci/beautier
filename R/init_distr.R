#' Initializes a distribution
#' @param distr a distribution,
#' using \code{\link{create_distr}}
#' @param distr_id the first distribution's ID
#' @param param_id the first parameter's ID
#' @return an initialized distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_distr <- function(
  distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_distr(distr))

  if (beautier::is_one_na(distr$id)) {
    distr$id <- distr_id
  }

  if (beautier::is_beta_distr(distr)) {

    if (beautier::is_one_na(distr$alpha$id)) {
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (beautier::is_one_na(distr$beta$id)) {
      distr$beta$id <- param_id
    }

  } else if (beautier::is_exp_distr(distr)) {

    if (beautier::is_one_na(distr$mean$id)) {
      distr$mean$id <- param_id
      param_id <- param_id + 1
    }

  } else if (beautier::is_gamma_distr(distr)) {

    if (beautier::is_one_na(distr$alpha$id)) {
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (beautier::is_one_na(distr$beta$id)) {
      distr$beta$id <- param_id
    }

  } else if (beautier::is_inv_gamma_distr(distr)) {

    if (beautier::is_one_na(distr$alpha$id)) {
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (beautier::is_one_na(distr$beta$id)) {
      distr$beta$id <- param_id
    }

  } else if (beautier::is_laplace_distr(distr)) {

    if (beautier::is_one_na(distr$mu$id)) {
      distr$mu$id <- param_id
      param_id <- param_id + 1
    }
    if (beautier::is_one_na(distr$scale$id)) {
      distr$scale$id <- param_id
    }

  } else if (beautier::is_log_normal_distr(distr)) {

    testit::assert("m" %in% names(distr))
    testit::assert("id" %in% names(distr$m))
    if (beautier::is_one_na(distr$m$id)) {
      distr$m$id <- param_id
      param_id <- param_id + 1
    }
    if (beautier::is_one_na(distr$s$id)) {
      distr$s$id <- param_id
    }

  } else if (beautier::is_normal_distr(distr)) {

    if (beautier::is_one_na(distr$mean$id)) {
      distr$mean$id <- param_id
      param_id <- param_id + 1
    }
    if (beautier::is_one_na(distr$sigma$id)) {
      distr$sigma$id <- param_id
    }

  } else if (beautier::is_one_div_x_distr(distr)) {

    # Always initialized

  } else if (beautier::is_poisson_distr(distr)) {

    if (beautier::is_one_na(distr$lambda$id)
    ) {
      distr$lambda$id <- param_id
    }

  } else {
    testit::assert(beautier::is_uniform_distr(distr))

    # Always initialized

  }
  testit::assert(!beautier::is_one_na(distr$id))
  distr
}
