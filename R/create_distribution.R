#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{\link{get_distribution_names}}
#' @param ... specific distribution parameters
#' @note Prefer using the
#'   named functions \code{\link{create_uniform_distribution}},
#'   and \code{\link{create_normal_distribution}}
#' @return a distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_distribution <- function(
  name,
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
) {
  return(
    beautier::create_distribution(
      name = "uniform"
    )
  )
}

#' Create an normal distribution
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
#' @return an exponential distribution
#' @export
create_exponential_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "exponential"
    )
  )
}

#' Create a gamma distribution
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
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
#' @inheritParams create_distribution
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
