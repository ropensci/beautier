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
    return(
      init_beta_distr(
        beta_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_exp_distr(distr)) {

    if (beautier::is_one_na(distr$mean$id)) {
      distr$mean$id <- param_id
      param_id <- param_id + 1
    }

  } else if (beautier::is_gamma_distr(distr)) {
    return(
      init_gamma_distr(
        gamma_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_inv_gamma_distr(distr)) {
    return(
      init_inv_gamma_distr(
        inv_gamma_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
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

#' Initializes a beta distribution
#' @inheritParams init_distr
#' @param beta_distr a beta distribution,
#' using \link{create_beta_distr}
#' @return an initialized beta distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_beta_distr <- function(
  beta_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_beta_distr(beta_distr))
  if (beautier::is_one_na(beta_distr$id)) {
    beta_distr$id <- distr_id
  }
  if (beautier::is_one_na(beta_distr$alpha$id)) {
    beta_distr$alpha$id <- param_id
    param_id <- param_id + 1
  }
  if (beautier::is_one_na(beta_distr$beta$id)) {
    beta_distr$beta$id <- param_id
  }
  beta_distr
}

#' Initializes a gamma distribution
#' @inheritParams init_distr
#' @param gamma_distr a gamma distribution,
#' using \link{create_gamma_distr}
#' @return an initialized gamma distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_gamma_distr <- function(
  gamma_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_gamma_distr(gamma_distr))
  if (beautier::is_one_na(gamma_distr$id)) {
    gamma_distr$id <- distr_id
  }
  if (beautier::is_one_na(gamma_distr$alpha$id)) {
    gamma_distr$alpha$id <- param_id
    param_id <- param_id + 1
  }
  if (beautier::is_one_na(gamma_distr$beta$id)) {
    gamma_distr$beta$id <- param_id
  }
  gamma_distr
}

#' Initializes an inverse gamma distribution
#' @inheritParams init_distr
#' @param inv_gamma_distr an inverse gamma distribution,
#' using \link{create_inv_gamma_distr}
#' @return an initialized inverse gamma distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_inv_gamma_distr <- function(
  inv_gamma_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_inv_gamma_distr(inv_gamma_distr))
  if (beautier::is_one_na(inv_gamma_distr$id)) {
    inv_gamma_distr$id <- distr_id
  }
  if (beautier::is_one_na(inv_gamma_distr$alpha$id)) {
    inv_gamma_distr$alpha$id <- param_id
    param_id <- param_id + 1
  }
  if (beautier::is_one_na(inv_gamma_distr$beta$id)) {
    inv_gamma_distr$beta$id <- param_id
  }
  inv_gamma_distr
}
