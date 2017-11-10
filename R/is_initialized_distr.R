#' Determine if x is an initialized distribution object
#'   as created by \code{\link{create_distr}}
#' @param x the object to check if it is an
#'   initialized distribution object
#' @return TRUE if x is an initialized distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_distr <- function(
  x
) {
  if (!beautier::is_distr(x)) return(FALSE)
  if (is.na(x$id)) return(FALSE)

  if (beautier::is_beta_distr(x)) {
    return(is_initialized_beta_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_exponential_distr(x)) {
    return(is_initialized_exponential_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_gamma_distr(x)) {
    return(is_initialized_gamma_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_inv_gamma_distr(x)) {
    return(is_initialized_inv_gamma_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_laplace_distr(x)) {
    return(is_initialized_laplace_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_log_normal_distr(x)) {
    return(is_initialized_log_normal_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_normal_distr(x)) {
    return(is_initialized_normal_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_one_div_x_distr(x)) {
    return(is_initialized_one_div_x_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_poisson_distr(x)) {
    return(is_initialized_poisson_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else {
    testit::assert(beautier::is_uniform_distr(x))
    return(is_initialized_uniform_distr(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  }
}

#' Determine if x is an initialized beta distribution object
#'   as created by \code{\link{create_beta_distr}}
#' @param x the object to check if it is an
#'   initialized beta distribution object
#' @return TRUE if x is an initialized beta distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_beta_distr <- function(
  x
) {
  testit::assert(beautier::is_beta_distr(x))
  !is.na(x$alpha$id) && !is.na(x$beta$id)
}

#' Determine if x is an initialized exponential distribution object
#'   as created by \code{\link{create_exponential_distr}}
#' @param x the object to check if it is an
#'   initialized exponential distribution object
#' @return TRUE if x is an initialized exponential distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_exponential_distr <- function(
  x
) {
  testit::assert(beautier::is_exponential_distr(x))
  !is.na(x$mean$id)
}


#' Determine if x is an initialized gamma distribution object
#' @param x the object to check if it is an
#'   initialized gamma distribution object
#' @return TRUE if x is an initialized gamma distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_gamma_distr <- function(
  x
) {
  testit::assert(beautier::is_gamma_distr(x))
  return(!is.na(x$alpha$id) && !is.na(x$beta$id))
}

#' Determine if x is an initialized inv_gamma distribution object
#'   as created by \code{\link{create_inv_gamma_distr}}
#' @param x the object to check if it is an
#'   initialized inv_gamma distribution object
#' @return TRUE if x is an initialized inv_gamma distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_inv_gamma_distr <- function(
  x
) {
  testit::assert(beautier::is_inv_gamma_distr(x))
  return(!is.na(x$alpha$id) && !is.na(x$beta$id))
  TRUE
}

#' Determine if x is an initialized laplace distribution object
#'   as created by \code{\link{create_laplace_distr}}
#' @param x the object to check if it is an
#'   initialized laplace distribution object
#' @return TRUE if x is an initialized laplace distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_laplace_distr <- function(
  x
) {
  testit::assert(beautier::is_laplace_distr(x))
  return(!is.na(x$mu$id) && !is.na(x$scale$id))
}

#' Determine if x is an initialized log_normal distribution object
#'   as created by \code{\link{create_log_normal_distr}}
#' @param x the object to check if it is an
#'   initialized log_normal distribution object
#' @return TRUE if x is an initialized log_normal distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_log_normal_distr <- function(
  x
) {
  testit::assert(beautier::is_log_normal_distr(x))
  return(!is.na(x$m$id) && !is.na(x$s$id))
}

#' Determine if x is an initialized normal distribution object
#'   as created by \code{\link{create_normal_distr}}
#' @param x the object to check if it is an
#'   initialized normal distribution object
#' @return TRUE if x is an initialized normal distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_normal_distr <- function(
  x
) {
  testit::assert(beautier::is_normal_distr(x))
  return(!is.na(x$mean$id) && !is.na(x$sigma$id))
}

#' Determine if x is an initialized one_div_x distribution object
#'   as created by \code{\link{create_one_div_x_distr}}
#' @param x the object to check if it is an
#'   initialized one_div_x distribution object
#' @return TRUE if x is an initialized one_div_x distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_one_div_x_distr <- function(
  x
) {
  testit::assert(beautier::is_one_div_x_distr(x))
  TRUE
}

#' Determine if x is an initialized poisson distribution object
#'   as created by \code{\link{create_poisson_distr}}
#' @param x the object to check if it is an
#'   initialized poisson distribution object
#' @return TRUE if x is an initialized poisson distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_poisson_distr <- function(
  x
) {
  testit::assert(beautier::is_poisson_distr(x))
  return(!is.na(x$m$id) && !is.na(x$s$id))
}

#' Determine if x is an initialized uniform distribution object
#'   as created by \code{\link{create_uniform_distr}}
#' @param x the object to check if it is an
#'   initialized uniform distribution object
#' @return TRUE if x is an initialized uniform distribution object
#' @author Richel J.C. Bilderbeek
is_initialized_uniform_distr <- function(
  x
) {
  testit::assert(beautier::is_uniform_distr(x))
  TRUE
}
