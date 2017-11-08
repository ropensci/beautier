#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{\link{get_distribution_names}}
#' @param id the distribution's ID
#' @param ... specific distribution parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_uniform_distr}},
#'   \code{\link{create_normal_distr}},
#'   \code{\link{create_one_div_x_distribution}},
#'   \code{\link{create_exponential_distr}},
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
  id,
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
create_uniform_distr <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "uniform",
      id = id
    )
  )
}

#' Create an normal distribution
#' @inheritParams create_distribution
#' @return a normal distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_normal_distr <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "normal",
      id = id
    )
  )
}

#' Create a 1/x distribution
#' @inheritParams create_distribution
#' @return a 1/x distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_one_div_x_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "one_div_x",
      id = id
    )
  )
}

#' Create a log-normal distribution
#' @inheritParams create_distribution
#' @return a log-normal distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_log_normal_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "log_normal",
      id = id
    )
  )
}

#' Create an exponential distribution
#' @inheritParams create_distribution
#' @return an exponential distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_exponential_distr <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "exponential",
      id = id
    )
  )
}

#' Create a gamma distribution
#' @inheritParams create_distribution
#' @return a gamma distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_gamma_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "gamma",
      id = id
    )
  )
}

#' Create a beta distribution
#' @inheritParams create_distribution
#' @return a beta distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_beta_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "beta",
      id = id
    )
  )
}

#' Create a Laplace distribution
#' @inheritParams create_distribution
#' @return a Laplace distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_laplace_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "laplace",
      id = id
    )
  )
}

#' Create an inverse gamma distribution
#' @inheritParams create_distribution
#' @return an inverse gamma distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_inv_gamma_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "inv_gamma",
      id = id
    )
  )
}

#' Create a Poisson distribution
#' @inheritParams create_distribution
#' @return a Poisson distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_poisson_distribution <- function(
  id = NA
) {
  return(
    beautier::create_distribution(
      name = "poisson",
      id = id
    )
  )
}
