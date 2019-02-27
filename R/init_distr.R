#' Initializes a distribution
#' @param distr a distribution,
#' using \code{\link{create_distr}}
#' @param distr_id the first distribution's ID
#' @param param_id the first parameter's ID
#' @return an initialized distribution
#' @author Richèl J.C. Bilderbeek
#' @noRd
init_distr <- function(
  distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_distr(distr)) # nolint beautier function

  if (is_one_na(distr$id)) { # nolint beautier function
    distr$id <- distr_id
  }

  if (is_beta_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$alpha$id)) { # nolint beautier function
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is_one_na(distr$beta$id)) { # nolint beautier function
      distr$beta$id <- param_id
    }

  } else if (is_exp_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$mean$id)) { # nolint beautier function
      distr$mean$id <- param_id
      param_id <- param_id + 1
    }

  } else if (is_gamma_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$alpha$id)) { # nolint beautier function
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is_one_na(distr$beta$id)) { # nolint beautier function
      distr$beta$id <- param_id
    }

  } else if (is_inv_gamma_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$alpha$id)) { # nolint beautier function
      distr$alpha$id <- param_id
      param_id <- param_id + 1
    }
    if (is_one_na(distr$beta$id)) { # nolint beautier function
      distr$beta$id <- param_id
    }

  } else if (is_laplace_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$mu$id)) { # nolint beautier function
      distr$mu$id <- param_id
      param_id <- param_id + 1
    }
    if (is_one_na(distr$scale$id)) { # nolint beautier function
      distr$scale$id <- param_id
    }

  } else if (is_log_normal_distr(distr)) { # nolint beautier function

    testit::assert("m" %in% names(distr))
    testit::assert("id" %in% names(distr$m))
    if (is_one_na(distr$m$id)) { # nolint beautier function
      distr$m$id <- param_id
      param_id <- param_id + 1
    }
    if (is_one_na(distr$s$id)) { # nolint beautier function
      distr$s$id <- param_id
    }

  } else if (is_normal_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$mean$id)) { # nolint beautier function
      distr$mean$id <- param_id
      param_id <- param_id + 1
    }
    if (is_one_na(distr$sigma$id)) { # nolint beautier function
      distr$sigma$id <- param_id
    }

  } else if (is_one_div_x_distr(distr)) { # nolint beautier function

    # Always initialized

  } else  if (is_poisson_distr(distr)) { # nolint beautier function

    if (is_one_na(distr$lambda$id)) { # nolint beautier function
      distr$lambda$id <- param_id
    }

  } else {
    testit::assert(is_uniform_distr(distr)) # nolint beautier function

    # Always initialized

  }
  testit::assert(!is_one_na(distr$id)) # nolint beautier function
  distr
}
