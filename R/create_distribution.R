#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{\link{get_distribution_names}}
#' @param id the distribution's ID
#' @param ... specific distribution parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_uniform_distribution}},
#'   \code{\link{create_normal_distribution}},
#'   \code{\link{create_one_div_x_distribution}},
#'   \code{\link{create_exponential_distribution}},
#'   \code{\link{create_gamma_distribution}},
#'   \code{\link{create_beta_distribution}},
#'   \code{\link{create_laplace_distribution}},
#'   \code{\link{create_inv_gamma_distribution}},
#'   and \code{\link{create_poisson_distribution}}
#' @return a distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_distribution <- function(
  name,
  id = NA,
  ...
) {
  if (!is_distribution_name(name)) {
    distributions_as_string <- function() {
      s <- NULL
      for (p in get_distribution_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid distribution name, must be one these: ",
      distributions_as_string()
    )
  }
  distribution <- list(
    name = name,
    id = id,
    ...
  )
  distribution
}

#' Create a uniform distribution
#' @inheritParams create_distribution
#' @return a uniform distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_uniform_distribution <- function(
  id = 1
) {
  return(
    beautier::create_distribution(
      name = "uniform",
      id = id
    )
  )
}

#' Create an normal distribution
#' @return a normal distribution
#' @export
create_normal_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "normal"
    )
  )
}

#' Create a 1/x distribution
#' @return a 1/x distribution
#' @export
create_one_div_x_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "one_div_x"
    )
  )
}

#' Create a log-normal distribution
#' @return a log-normal distribution
#' @export
create_log_normal_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "log_normal"
    )
  )
}

#' Create an exponential distribution
#' @return an exponential distribution
#' @export
create_exponential_distr <- function(
) {
  return(
    beautier::create_distribution(
      name = "exponential"
    )
  )
}

#' Create a gamma distribution
#' @return a gamma distribution
#' @export
create_gamma_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "gamma"
    )
  )
}

#' Create a beta distribution
#' @return a beta distribution
#' @export
create_beta_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "beta"
    )
  )
}

#' Create a Laplace distribution
#' @return a Laplace distribution
#' @export
create_laplace_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "laplace"
    )
  )
}

#' Create an inverse gamma distribution
#' @return an inverse gamma distribution
#' @export
create_inv_gamma_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "inv_gamma"
    )
  )
}

#' Create a Poisson distribution
#' @return a Poisson distribution
#' @export
create_poisson_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "poisson"
    )
  )
}
