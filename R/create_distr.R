#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{\link{get_distr_names}}
#' @param id the distribution's ID
#' @param ... specific distribution parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_beta_distr}},
#'   \code{\link{create_exponential_distr}},
#'   \code{\link{create_gamma_distr}},
#'   \code{\link{create_inv_gamma_distr}},
#'   \code{\link{create_laplace_distr}},
#'   \code{\link{create_log_normal_distr}},
#'   \code{\link{create_normal_distr}},
#'   \code{\link{create_one_div_x_distr}},
#'   \code{\link{create_poisson_distr}}
#'   and \code{\link{create_uniform_distr}}
#' @return a distribution
#' @seealso use \code{\link{is_distr}} to check if a
#'   distribution is valid
#' @author Richel J.C. Bilderbeek
#' @export
create_distr <- function(
  name,
  id,
  ...
) {
  if (!is_distr_name(name)) {
    distr_as_string <- function() {
      s <- NULL
      for (p in get_distr_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid distribution name, must be one these: ",
      distr_as_string()
    )
  }
  distr <- list(
    name = name,
    id = id,
    ...
  )
  distr
}


#' Create a beta distribution
#' @inheritParams create_distr
#' @param alpha the alpha shape parameter
#' @param beta the beta shape parameter
#' @return a beta distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_beta_distr <- function(
  id = NA,
  alpha = create_alpha_parameter(id = NA, estimate = NA, value = NA),
  beta = create_beta_parameter(id = NA, estimate = NA, value = NA)
) {
  return(
    beautier::create_distr(
      name = "beta",
      id = id,
      alpha = alpha,
      beta = beta
    )
  )
}

#' Create an exponential distribution
#' @inheritParams create_distr
#' @param mean the mean parameter
#' @return an exponential distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_exponential_distr <- function(
  id = NA,
  mean = create_mean_parameter(id = NA, estimate = NA, value = NA)
) {
  return(
    beautier::create_distr(
      name = "exponential",
      id = id,
      mean = mean
    )
  )
}

#' Create a gamma distribution
#' @inheritParams create_distr
#' @param alpha the alpha shape parameter
#' @param beta the beta shape parameter
#' @return a gamma distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_gamma_distr <- function(
  id = NA,
  alpha = create_alpha_parameter(id = NA, estimate = FALSE, value = "0.5396"),
  beta = create_beta_parameter(id = NA, estimate = FALSE, value = "0.3819")
) {
  return(
    beautier::create_distr(
      name = "gamma",
      id = id,
      alpha = alpha,
      beta = beta
    )
  )
}

#' Create an inverse gamma distribution
#' @inheritParams create_distr
#' @return an inverse gamma distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_inv_gamma_distr <- function(
  id = NA,
  alpha = create_alpha_parameter(),
  beta = create_beta_parameter()
) {
  return(
    beautier::create_distr(
      name = "inv_gamma",
      id = id,
      alpha = alpha,
      beta = beta
    )
  )
}

#' Create a Laplace distribution
#' @inheritParams create_distr
#' @param mu the mu parameter
#' @param scale the scale parameter
#' @return a Laplace distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_laplace_distr <- function(
  id = NA,
  mu = create_mu_parameter(id = NA, estimate = FALSE, value = NA),
  scale = create_scale_parameter(id = NA, estimate = FALSE, value = NA)
) {
  return(
    beautier::create_distr(
      name = "laplace",
      id = id,
      mu = mu,
      scale = scale
    )
  )
}

#' Create a log-normal distribution
#' @inheritParams create_distr
#' @return a log-normal distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_log_normal_distr <- function(
  id = NA,
  m = create_m_parameter(),
  s = create_s_parameter()
) {
  return(
    beautier::create_distr(
      name = "log_normal",
      id = id,
      m = m,
      s = s
    )
  )
}

#' Create an normal distribution
#' @inheritParams create_distr
#' @return a normal distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_normal_distr <- function(
  id = NA,
  mean = create_mean_parameter(),
  sigma = create_sigma_parameter()
) {
  return(
    beautier::create_distr(
      name = "normal",
      id = id,
      mean = mean,
      sigma = sigma
    )
  )
}

#' Create a 1/x distribution
#' @inheritParams create_distr
#' @return a 1/x distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_one_div_x_distr <- function(
  id = NA
) {
  return(
    beautier::create_distr(
      name = "one_div_x",
      id = id
    )
  )
}

#' Create a Poisson distribution
#' @inheritParams create_distr
#' @return a Poisson distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_poisson_distr <- function(
  id = NA,
  m = create_m_parameter(),
  s = create_s_parameter()
) {
  return(
    beautier::create_distr(
      name = "poisson",
      id = id,
      m = m,
      s = s
    )
  )
}

#' Create a uniform distribution
#' @inheritParams create_distr
#' @param upper an upper limit of the uniform distribution
#' @return a uniform distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_uniform_distr <- function(
  id = NA,
  upper = Inf
) {
  return(
    beautier::create_distr(
      name = "uniform",
      id = id,
      upper = upper
    )
  )
}
