#' Initializes a distribution
#' @param distr a distribution,
#' using \code{\link{create_distr}}
#' @param distr_id the first distribution's ID
#' @param param_id the first parameter's ID
#' @return an initialized distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_distr <- function( # nolint simplifying this more hurts readability
  distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_distr(distr))
  if (beautier::is_beta_distr(distr)) {
    return(
      beautier::init_beta_distr(
        beta_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_exp_distr(distr)) {
    return(
      beautier::init_exp_distr(
        exp_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_gamma_distr(distr)) {
    return(
      beautier::init_gamma_distr(
        gamma_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_inv_gamma_distr(distr)) {
    return(
      beautier::init_inv_gamma_distr(
        inv_gamma_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_laplace_distr(distr)) {
    return(
      beautier::init_laplace_distr(
        laplace_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_log_normal_distr(distr)) {
    return(
      beautier::init_log_normal_distr(
        log_normal_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  } else if (beautier::is_normal_distr(distr)) {
    return(
      beautier::init_normal_distr(
        normal_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )

  } else if (beautier::is_one_div_x_distr(distr)) {
    return(
      beautier::init_one_div_x_distr(
        one_div_x_distr = distr,
        distr_id = distr_id
      )
    )
  } else if (beautier::is_poisson_distr(distr)) {
    return(
      beautier::init_poisson_distr(
        poisson_distr = distr,
        distr_id = distr_id,
        param_id = param_id
      )
    )
  }
  testit::assert(beautier::is_uniform_distr(distr))
  beautier::init_uniform_distr(
    uniform_distr = distr,
    distr_id = distr_id
  )
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
  testit::assert(beautier::is_beta_distr(beta_distr))

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

#' Initializes an exponential distribution
#' @inheritParams init_distr
#' @param exp_distr a exponential distribution,
#' using \link{create_exp_distr}
#' @return an initialized exponential distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_exp_distr <- function(
  exp_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_exp_distr(exp_distr))

  if (beautier::is_one_na(exp_distr$id)) {
    exp_distr$id <- distr_id
  }
  if (beautier::is_one_na(exp_distr$mean$id)) {
    exp_distr$mean$id <- param_id
  }
  exp_distr
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
  testit::assert(beautier::is_gamma_distr(gamma_distr))

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
  testit::assert(beautier::is_inv_gamma_distr(inv_gamma_distr))

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

#' Initializes an Laplace distribution
#' @inheritParams init_distr
#' @param laplace_distr a Laplace distribution,
#' using \link{create_laplace_distr}
#' @return an initialized Laplace distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_laplace_distr <- function(
  laplace_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_laplace_distr(laplace_distr))

  if (beautier::is_one_na(laplace_distr$id)) {
    laplace_distr$id <- distr_id
  }
  if (beautier::is_one_na(laplace_distr$mu$id)) {
    laplace_distr$mu$id <- param_id
    param_id <- param_id + 1
  }
  if (beautier::is_one_na(laplace_distr$scale$id)) {
    laplace_distr$scale$id <- param_id
  }
  laplace_distr
}

#' Initializes an log-normal distribution
#' @inheritParams init_distr
#' @param log_normal_distr a log-normal distribution,
#' using \link{create_log_normal_distr}
#' @return an initialized log-normal distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_log_normal_distr <- function(
  log_normal_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_log_normal_distr(log_normal_distr))

  if (beautier::is_one_na(log_normal_distr$id)) {
    log_normal_distr$id <- distr_id
  }
  if (beautier::is_one_na(log_normal_distr$m$id)) {
    log_normal_distr$m$id <- param_id
    param_id <- param_id + 1
  }
  if (beautier::is_one_na(log_normal_distr$s$id)) {
    log_normal_distr$s$id <- param_id
  }
  log_normal_distr
}

#' Initializes an normal distribution
#' @inheritParams init_distr
#' @param normal_distr a normal distribution,
#' using \link{create_normal_distr}
#' @return an initialized normal distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_normal_distr <- function(
  normal_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_normal_distr(normal_distr))

  if (beautier::is_one_na(normal_distr$id)) {
    normal_distr$id <- distr_id
  }
  if (beautier::is_one_na(normal_distr$mean$id)) {
    normal_distr$mean$id <- param_id
    param_id <- param_id + 1
  }
  if (beautier::is_one_na(normal_distr$sigma$id)) {
    normal_distr$sigma$id <- param_id
  }
  normal_distr
}

#' Initializes an one-divided-by-x distribution
#' @inheritParams init_distr
#' @param one_div_x_distr a one-divided-by-x distribution,
#' using \link{create_one_div_x_distr}
#' @return an initialized one-divided-by-x distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_one_div_x_distr <- function(
  one_div_x_distr,
  distr_id = 0
) {
  testit::assert(beautier::is_one_div_x_distr(one_div_x_distr))

  if (beautier::is_one_na(one_div_x_distr$id)) {
    one_div_x_distr$id <- distr_id
  }
  one_div_x_distr
}

#' Initializes an Poisson distribution
#' @inheritParams init_distr
#' @param poisson_distr a Poisson distribution,
#' using \link{create_poisson_distr}
#' @return an initialized Poisson distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_poisson_distr <- function(
  poisson_distr,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_poisson_distr(poisson_distr))

  if (beautier::is_one_na(poisson_distr$id)) {
    poisson_distr$id <- distr_id
  }
  if (beautier::is_one_na(poisson_distr$lambda$id)) {
    poisson_distr$lambda$id <- param_id
  }
  poisson_distr
}

#' Initializes a uniform distribution
#' @inheritParams init_distr
#' @param uniform_distr a uniform distribution,
#' using \link{create_uniform_distr}
#' @return an initialized uniform distribution
#' @author Richèl J.C. Bilderbeek
#' @export
init_uniform_distr <- function(
  uniform_distr,
  distr_id = 0
) {
  testit::assert(beautier::is_uniform_distr(uniform_distr))

  if (beautier::is_one_na(uniform_distr$id)) {
    uniform_distr$id <- distr_id
  }
  uniform_distr
}
