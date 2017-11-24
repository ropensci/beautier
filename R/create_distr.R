#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{\link{get_distr_names}}
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
#' @param alpha the alpha shape parameter,
#'   as returned by \code{\link{create_alpha_param}}
#' @param beta the beta shape parameter,
#'   as returned by \code{\link{create_beta_param}}
#' @return a beta distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @examples
#'   beta_distr <- create_beta_distr()
#'   testit::assert(is_beta_distr(beta_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = beta_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_beta_distr <- function(
  id = NA,
  alpha = create_alpha_param(),
  beta = create_beta_param()
) {
  if (!is_alpha_param(alpha)) {
    stop("'alpha' must be an alpha parameter, ",
      "as returned by 'create_alpha_param'")
  }
  if (!is_beta_param(beta)) {
    stop("'beta' must be a beta parameter, ",
      "as returned by 'create_beta_param'")
  }
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
#' @param mean the mean parameter,
#'   as returned by \code{\link{create_mean_param}}
#' @return an exponential distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @author Richel J.C. Bilderbeek
#' @examples
#'   exp_distr <- create_exp_distr()
#'   testit::assert(is_exp_distr(exp_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = exp_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_exp_distr <- function(
  id = NA,
  mean = create_mean_param(id = NA, estimate = NA, value = NA)
) {
  if (!is_mean_param(mean)) {
    stop("'mean' must be a mean parameter, ",
      "as returned by 'create_mean_param'")
  }
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
#' @param alpha the alpha shape parameter,
#'   as returned by \code{\link{create_alpha_param}}
#' @param beta the beta shape parameter,
#'   as returned by \code{\link{create_beta_param}}
#' @return a gamma distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @examples
#'   gamma_distr <- create_gamma_distr()
#'   testit::assert(is_gamma_distr(gamma_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = gamma_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_gamma_distr <- function(
  id = NA,
  alpha = create_alpha_param(id = NA, estimate = FALSE, value = "0.5396"),
  beta = create_beta_param(id = NA, estimate = FALSE, value = "0.3819")
) {
  if (!is_alpha_param(alpha)) {
    stop("'alpha' must be an alpha parameter, ",
      "as returned by 'create_alpha_param'")
  }
  if (!is_beta_param(beta)) {
    stop("'beta' must be a beta parameter, ",
      "as returned by 'create_beta_param'")
  }
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
#' @param alpha the alpha shape parameter,
#'   as returned by \code{\link{create_alpha_param}}
#' @param beta the beta shape parameter,
#'   as returned by \code{\link{create_beta_param}}
#' @return an inverse gamma distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @examples
#'   inv_gamma_distr <- create_inv_gamma_distr()
#'   testit::assert(is_inv_gamma_distr(inv_gamma_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = inv_gamma_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_inv_gamma_distr <- function(
  id = NA,
  alpha = create_alpha_param(),
  beta = create_beta_param()
) {
  if (!is_alpha_param(alpha)) {
    stop("'alpha' must be an alpha parameter, ",
      "as returned by 'create_alpha_param'")
  }
  if (!is_beta_param(beta)) {
    stop("'beta' must be a beta parameter, ",
      "as returned by 'create_beta_param'")
  }
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
#' @param mu the mu parameter,
#'   as returned by \code{\link{create_mu_param}}
#' @param scale the scale parameter,
#'   as returned by \code{\link{create_scale_param}}
#' @return a Laplace distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @examples
#'   laplace_distr <- create_laplace_distr()
#'   testit::assert(is_laplace_distr(laplace_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = laplace_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_laplace_distr <- function(
  id = NA,
  mu = create_mu_param(id = NA, estimate = FALSE, value = 0.0),
  scale = create_scale_param(id = NA, estimate = FALSE, value = 1.0)
) {
  if (!beautier::is_mu_param(mu)) {
    stop("'mu' must be a mu parameter, ",
      "as returned by 'create_mu_param'")
  }
  if (!beautier::is_scale_param(scale)) {
    stop("'scale' must be an scale parameter, ",
      "as returned by 'create_scale_param'")
  }
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
#' @param m the m parameter,
#'   as returned by \code{\link{create_m_param}}
#' @param s the s parameter,
#'   as returned by \code{\link{create_s_param}}
#' @return a log-normal distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @examples
#'   log_normal_distr <- create_log_normal_distr()
#'   testit::assert(is_log_normal_distr(log_normal_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = log_normal_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_log_normal_distr <- function(
  id = NA,
  m = create_m_param(),
  s = create_s_param()
) {
  if (!is_m_param(m)) {
    stop("'m' must be an m parameter, as returned by 'create_m_param'")
  }
  if (!is_s_param(s)) {
    stop("'s' must be an s parameter, as returned by 'create_s_param'")
  }
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
#' @param mean the mean parameter
#'   as returned by \code{\link{create_mean_param}}
#' @param sigma the sigma parameter
#'   as returned by \code{\link{create_sigma_param}}
#' @return a normal distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @examples
#'   normal_distr <- create_normal_distr()
#'   testit::assert(is_normal_distr(normal_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = normal_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_normal_distr <- function(
  id = NA,
  mean = create_mean_param(),
  sigma = create_sigma_param()
) {
  if (!is_mean_param(mean)) {
    stop("'mean' must be a mean parameter, ",
      "as returned by 'create_mean_param'")
  }
  if (!is_sigma_param(sigma)) {
    stop("'sigma' must be a sigma parameter, ",
      "as returned by 'create_sigma_param'")
  }
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
#' @examples
#'   one_div_x_distr <- create_one_div_x_distr()
#'   testit::assert(is_one_div_x_distr(one_div_x_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = one_div_x_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
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
#' @param lambda the lambda parameter
#'   as returned by \code{\link{create_lambdaparam}}
#' @return a Poisson distribution
#' @seealso the function \code{\link{create_distr}} shows an overview
#'   of all supported distributions
#' @author Richel J.C. Bilderbeek
#' @export
create_poisson_distr <- function(
  id = NA,
  lambda = create_lambdaparam()
) {
  if (!is_lambdaparam(lambda)) {
    stop("'lambda' must be a lambda parameter, ",
      "as returned by 'create_lambdaparam'")
  }
  return(
    beautier::create_distr(
      name = "poisson",
      id = id,
      lambda = lambda
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
#' @examples
#'   uniform_distr <- create_uniform_distr()
#'   testit::assert(is_uniform_distr(uniform_distr))
#'
#'   input_fasta_filename <- system.file(
#'     "extdata", "anthus_aco.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = uniform_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
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
