#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{get_distr_names}
#' @param id the distribution's ID
#' @param ... specific distribution parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_beta_distr}},
#'   \code{\link{create_exp_distr}},
#'   \code{\link{create_gamma_distr}},
#'   \code{\link{create_inv_gamma_distr}},
#'   \code{\link{create_laplace_distr}},
#'   \code{\link{create_log_normal_distr}},
#'   \code{\link{create_normal_distr}},
#'   \code{\link{create_one_div_x_distr}},
#'   \code{\link{create_poisson_distr}}
#'   and \code{\link{create_uniform_distr}}
#' @return a distribution
#' @note See
#'   \code{\link{create_beta_distr}},
#'   \code{\link{create_exp_distr}},
#'   \code{\link{create_gamma_distr}},
#'   \code{\link{create_inv_gamma_distr}},
#'   \code{\link{create_laplace_distr}},
#'   \code{\link{create_log_normal_distr}},
#'   \code{\link{create_normal_distr}},
#'   \code{\link{create_one_div_x_distr}},
#'   \code{\link{create_poisson_distr}}
#'   and \code{\link{create_uniform_distr}}
#'   for examples how to use those distributions
#' @examples
#'   # Use any distribution
#'   distr <- create_beta_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @author Richèl J.C. Bilderbeek
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
      "'name' must be a distribution name: ",
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
#' @param alpha the alpha shape parameter,
#'   a numeric value.
#'   The value
#'   of alpha must be at least 0.0.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_alpha_param}}.
#' @param beta the beta shape parameter,
#'   a numeric value.
#'   The value
#'   of beta must be at least 1.0.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_beta_param}}.
#' @return a beta distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   beta_distr <- create_beta_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = beta_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_beta_distr create_distr_beta
#' @export create_beta_distr create_distr_beta
create_beta_distr <- create_distr_beta <- function(
  id = NA,
  alpha = 0.0,
  beta = 1.0
) {
  if (length(alpha) == 1 && is.numeric(alpha)) {
    alpha <- create_alpha_param(value = alpha)
  }
  if (length(beta) == 1 && is.numeric(beta)) {
    beta <- create_beta_param(value = beta)
  }
  if (!is_alpha_param(alpha)) {
    stop("'alpha' must be an alpha parameter, ",
      "as returned by 'create_alpha_param'")
  }
  if (!is_beta_param(beta)) {
    stop("'beta' must be a beta parameter, ",
      "as returned by 'create_beta_param'")
  }
  if (alpha$value < 0.0) {
    stop("'alpha' must have a positive value")
  }
  if (beta$value < 1.0) {
    stop("'beta' must have a value of at least 1.0")
  }
  beautier::create_distr(
    name = "beta",
    id = id,
    alpha = alpha,
    beta = beta
  )
}

#' Create an exponential distribution
#' @inheritParams create_distr
#' @param mean the mean parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_mean_param}}
#' @return an exponential distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   exp_distr <- create_exp_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = exp_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_exp_distr create_distr_exp
#' @export create_exp_distr create_distr_exp
create_exp_distr <- create_distr_exp <- function(
  id = NA,
  mean = 1.0
) {
  if (length(mean) == 1 && is.numeric(mean)) {
    mean <- create_mean_param(value = mean)
  }
  if (!is_mean_param(mean)) {
    stop("'mean' must be a mean parameter, ",
      "as returned by 'create_mean_param'")
  }
  beautier::create_distr(
    name = "exponential",
    id = id,
    mean = mean
  )
}

#' Create a gamma distribution
#' @inheritParams create_distr
#' @param alpha the alpha shape parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_alpha_param}}
#' @param beta the beta shape parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_beta_param}}
#' @return a gamma distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   gamma_distr <- create_gamma_distr(
#'      alpha = 0.05,
#'      beta = 10.0
#'   )
#'
#'   gtr_site_model <- create_gtr_site_model(
#'     rate_ac_prior_distr = gamma_distr
#'   )
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     site_model = gtr_site_model
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_gamma_distr create_distr_gamma
#' @export create_gamma_distr create_distr_gamma
create_gamma_distr <- create_distr_gamma <- function(
  id = NA,
  alpha = 0.5396,
  beta = 0.3819
) {
  if (length(alpha) == 1 && is.numeric(alpha)) {
    alpha <- create_alpha_param(value = alpha)
  }
  if (length(beta) == 1 && is.numeric(beta)) {
    beta <- create_beta_param(value = beta)
  }
  if (!is_alpha_param(alpha)) {
    stop("'alpha' must be an alpha parameter, ",
      "as returned by 'create_alpha_param'")
  }
  if (!is_beta_param(beta)) {
    stop("'beta' must be a beta parameter, ",
      "as returned by 'create_beta_param'")
  }
  if (alpha$value < 0.0) {
    stop("'value' of 'alpha' must be positive")
  }
  if (beta$value < 0.0) {
    stop("'value' of 'beta' must be positive")
  }

  beautier::create_distr(
    name = "gamma",
    id = id,
    alpha = alpha,
    beta = beta
  )
}

#' Create an inverse-gamma distribution
#' @inheritParams create_distr
#' @param alpha the alpha shape parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_alpha_param}}
#' @param beta the beta shape parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_beta_param}}
#' @return an inverse-gamma distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   inv_gamma_distr <- create_inv_gamma_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = inv_gamma_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_inv_gamma_distr create_distr_inv_gamma
#' @export create_inv_gamma_distr create_distr_inv_gamma
create_inv_gamma_distr <- create_distr_inv_gamma <- function(
  id = NA,
  alpha = 0.0,
  beta = 1.0
) {
  if (length(alpha) == 1 && is.numeric(alpha)) {
    alpha <- create_alpha_param(value = alpha)
  }
  if (length(beta) == 1 && is.numeric(beta)) {
    beta <- create_beta_param(value = beta)
  }
  if (!is_alpha_param(alpha)) {
    stop("'alpha' must be an alpha parameter, ",
      "as returned by 'create_alpha_param'")
  }
  if (!is_beta_param(beta)) {
    stop("'beta' must be a beta parameter, ",
      "as returned by 'create_beta_param'")
  }
  beautier::create_distr(
    name = "inv_gamma",
    id = id,
    alpha = alpha,
    beta = beta
  )
}

#' Create a Laplace distribution
#' @inheritParams create_distr
#' @param mu the mu parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_mu_param}}
#' @param scale the scale parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_scale_param}}
#' @return a Laplace distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   laplace_distr <- create_laplace_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = laplace_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_laplace_distr create_distr_laplace
#' @export create_laplace_distr create_distr_laplace
create_laplace_distr <- create_distr_laplace <- function(
  id = NA,
  mu = 0.0,
  scale = 1.0
) {
  if (length(mu) == 1 && is.numeric(mu)) {
    mu <- create_mu_param(value = mu)
  }
  if (length(scale) == 1 && is.numeric(scale)) {
    scale <- create_scale_param(value = scale)
  }

  if (!is_mu_param(mu)) {
    stop("'mu' must be a mu parameter, ",
      "as returned by 'create_mu_param'")
  }
  if (!is_scale_param(scale)) {
    stop("'scale' must be a scale parameter, ",
      "as returned by 'create_scale_param'")
  }
  beautier::create_distr(
    name = "laplace",
    id = id,
    mu = mu,
    scale = scale
  )
}

#' Create a log-normal distribution
#' @inheritParams create_distr
#' @param m the m parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_m_param}}
#' @param s the s parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_s_param}}
#' @return a log-normal distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   log_normal_distr <- create_log_normal_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = log_normal_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_log_normal_distr create_distr_log_normal
#' @export create_log_normal_distr create_distr_log_normal
create_log_normal_distr <- create_distr_log_normal <- function(
  id = NA,
  m = 0.0,
  s = 0.0
) {
  if (length(m) == 1 && is.numeric(m)) {
    m <- create_m_param(value = m)
  }
  if (length(s) == 1 && is.numeric(s)) {
    s <- create_s_param(value = s)
  }
  if (!is_m_param(m)) {
    stop("'m' must be an m parameter, as returned by 'create_m_param'")
  }
  if (!is_s_param(s)) {
    stop("'s' must be an s parameter, as returned by 'create_s_param'")
  }
  if (s$value < 0.0) {
    stop("'value' of 's' must be positive")
  }
  beautier::create_distr(
    name = "log_normal",
    id = id,
    m = m,
    s = s
  )
}

#' Create an normal distribution
#' @inheritParams create_distr
#' @param mean the mean parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_mean_param}}
#' @param sigma the sigma parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_sigma_param}}
#' @return a normal distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   normal_distr <- create_normal_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = normal_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_normal_distr create_distr_normal
#' @export create_normal_distr create_distr_normal
create_normal_distr <- create_distr_normal <- function(
  id = NA,
  mean = 0.0,
  sigma = 1.0
) {
  if (length(mean) == 1 && is.numeric(mean)) {
    mean <- create_mean_param(value = mean)
  }
  if (length(sigma) == 1 && is.numeric(sigma)) {
    sigma <- create_sigma_param(value = sigma)
  }

  if (!is_mean_param(mean)) { # nolint beautier function
    stop("'mean' must be a mean parameter, ",
      "as returned by 'create_mean_param'")
  }
  if (!is_sigma_param(sigma)) { # nolint beautier function
    stop("'sigma' must be a sigma parameter, ",
      "as returned by 'create_sigma_param'")
  }
  beautier::create_distr( # nolint beautier function
    name = "normal",
    id = id,
    mean = mean,
    sigma = sigma
  )
}

#' Create a 1/x distribution
#' @inheritParams create_distr
#' @return a 1/x distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   one_div_x_distr <- create_one_div_x_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = one_div_x_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_one_div_x_distr create_distr_one_div_x
#' @export create_one_div_x_distr create_distr_one_div_x
create_one_div_x_distr <- create_distr_one_div_x <- function(
  id = NA
) {
  beautier::create_distr(
    name = "one_div_x",
    id = id
  )
}

#' Create a Poisson distribution
#' @inheritParams create_distr
#' @param lambda the lambda parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as returned by \code{\link{create_lambda_param}}
#' @return a Poisson distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   poisson_distr <- create_poisson_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = poisson_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_poisson_distr create_distr_poisson
#' @export create_poisson_distr create_distr_poisson
create_poisson_distr <- create_distr_poisson <- function(
  id = NA,
  lambda = 0.0
) {
  if (length(lambda) == 1 && is.numeric(lambda)) {
    lambda <- create_lambda_param(value = lambda)
  }

  if (!is_lambda_param(lambda)) {
    stop("'lambda' must be a lambda parameter, ",
      "as returned by 'create_lambda_param'")
  }
  beautier::create_distr(
    name = "poisson",
    id = id,
    lambda = lambda
  )
}

#' Create a uniform distribution
#' @inheritParams create_distr
#' @param upper an upper limit of the uniform distribution.
#'   If the upper limits needs to be infinity, set \code{upper} to \code{Inf}.
#' @return a uniform distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   uniform_distr <- create_uniform_distr()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = uniform_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_uniform_distr create_distr_uniform
#' @export create_uniform_distr create_distr_uniform
create_uniform_distr <- create_distr_uniform <- function(
  id = NA,
  upper = Inf
) {
  if (!is_one_na(upper) && upper <= 0.0) { # nolint beautier function
    stop("'upper' must be non-zero and positive")
  }
  beautier::create_distr(
    name = "uniform",
    id = id,
    upper = upper
  )
}
