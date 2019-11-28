#' Determine if x is an initialized distribution object
#'   as created by \code{\link{create_distr}}
#' @param x the object to check if it is an
#'   initialized distribution object
#' @return TRUE if x is an initialized distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_distr <- function(
  x
) {
  if (!beautier::is_distr(x)) return(FALSE)
  if (beautier::is_one_na(x$id)) return(FALSE)

  if (beautier::is_beta_distr(x)) {
    return(beautier::is_init_beta_distr(x))
  } else if (beautier::is_exp_distr(x)) {
    return(beautier::is_init_exp_distr(x))
  } else if (beautier::is_gamma_distr(x)) {
    return(beautier::is_init_gamma_distr(x))
  } else if (beautier::is_inv_gamma_distr(x)) {
    return(beautier::is_init_inv_gamma_distr(x))
  } else if (beautier::is_laplace_distr(x)) {
    return(beautier::is_init_laplace_distr(x))
  } else if (beautier::is_log_normal_distr(x)) {
    return(beautier::is_init_log_normal_distr(x))
  } else if (beautier::is_normal_distr(x)) {
    return(beautier::is_init_normal_distr(x))
  } else if (beautier::is_one_div_x_distr(x)) {
    return(beautier::is_init_one_div_x_distr(x))
  } else if (beautier::is_poisson_distr(x)) {
    return(beautier::is_init_poisson_distr(x))
  } else {
    testit::assert(beautier::is_uniform_distr(x))
    return(beautier::is_init_uniform_distr(x))
  }
}

#' Determine if x is an initialized beta distribution object
#'   as created by \code{\link{create_beta_distr}}
#' @param x the object to check if it is an
#'   initialized beta distribution object
#' @return TRUE if x is an initialized beta distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_beta_distr <- function(
  x
) {
  testit::assert(beautier::is_beta_distr(x))
  !beautier::is_one_na(x$alpha$id) && !beautier::is_one_na(x$beta$id)
}

#' Determine if x is an initialized exponential distribution object
#'   as created by \code{\link{create_exp_distr}}
#' @param x the object to check if it is an
#'   initialized exponential distribution object
#' @return TRUE if x is an initialized exponential distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_exp_distr <- function(
  x
) {
  testit::assert(beautier::is_exp_distr(x))
  !beautier::is_one_na(x$mean$id)
}

#' Determine if x is an initialized gamma distribution object
#' @param x the object to check if it is an
#'   initialized gamma distribution object
#' @return TRUE if x is an initialized gamma distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_gamma_distr <- function(
  x
) {
  testit::assert(beautier::is_gamma_distr(x))
  !beautier::is_one_na(x$alpha$id) && !beautier::is_one_na(x$beta$id)
}

#' Determine if x is an initialized inverse-gamma distribution
#'   as created by \code{\link{create_inv_gamma_distr}}
#' @param x the object to check if it is an
#'   initialized inverse-gamma distribution
#' @return TRUE if x is an initialized inverse-gamma distribution
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_inv_gamma_distr <- function(
  x
) {
  testit::assert(beautier::is_inv_gamma_distr(x))
  !beautier::is_one_na(x$alpha$id) && !beautier::is_one_na(x$beta$id)
}

#' Determine if x is an initialized Laplace distribution
#'   as created by \code{\link{create_laplace_distr}}
#' @param x the object to check if it is an
#'   initialized Laplace distribution
#' @return TRUE if x is an initialized Laplace distribution
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_laplace_distr <- function(
  x
) {
  testit::assert(beautier::is_laplace_distr(x))
  !beautier::is_one_na(x$mu$id) && !beautier::is_one_na(x$scale$id)
}

#' Determine if x is an initialized log_normal distribution object
#'   as created by \code{\link{create_log_normal_distr}}
#' @param x the object to check if it is an
#'   initialized log_normal distribution object
#' @return TRUE if x is an initialized log_normal distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_log_normal_distr <- function(
  x
) {
  testit::assert(beautier::is_log_normal_distr(x))
  !beautier::is_one_na(x$m$id) && !beautier::is_one_na(x$s$id)
}

#' Determine if x is an initialized normal distribution object
#'   as created by \code{\link{create_normal_distr}}
#' @param x the object to check if it is an
#'   initialized normal distribution object
#' @return TRUE if x is an initialized normal distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_normal_distr <- function(
  x
) {
  testit::assert(beautier::is_normal_distr(x))
  !beautier::is_one_na(x$mean$id) && !beautier::is_one_na(x$sigma$id)
}

#' Determine if x is an initialized one_div_x distribution object
#'   as created by \code{\link{create_one_div_x_distr}}
#' @param x the object to check if it is an
#'   initialized one_div_x distribution object
#' @return TRUE if x is an initialized one_div_x distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_one_div_x_distr <- function(
  x
) {
  testit::assert(beautier::is_one_div_x_distr(x))
  TRUE
}

#' Determine if x is an initialized Poisson distribution object
#'   as created by \code{\link{create_poisson_distr}}
#' @param x the object to check if it is an
#'   initialized Poisson distribution object
#' @return TRUE if x is an initialized Poisson distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_poisson_distr <- function(
  x
) {
  testit::assert(beautier::is_poisson_distr(x))
  !beautier::is_one_na(x$lambda$id)
}

#' Determine if x is an initialized uniform distribution object
#'   as created by \code{\link{create_uniform_distr}}
#' @param x the object to check if it is an
#'   initialized uniform distribution object
#' @return TRUE if x is an initialized uniform distribution object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_uniform_distr <- function(
  x
) {
  testit::assert(beautier::is_uniform_distr(x))
  TRUE
}
