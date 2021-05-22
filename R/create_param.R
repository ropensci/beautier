#' General function to create a parameter.
#' @inheritParams default_parameters_doc
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_alpha_param}},
#'   \code{\link{create_beta_param}},
#'   \code{\link{create_clock_rate_param}},
#'   \code{\link{create_kappa_1_param}},
#'   \code{\link{create_kappa_2_param}},
#'   \code{\link{create_lambda_param}},
#'   \code{\link{create_m_param}},
#'   \code{\link{create_mean_param}},
#'   \code{\link{create_mu_param}},
#'   \code{\link{create_rate_ac_param}},
#'   \code{\link{create_rate_ag_param}},
#'   \code{\link{create_rate_at_param}},
#'   \code{\link{create_rate_cg_param}},
#'   \code{\link{create_rate_ct_param}},
#'   \code{\link{create_rate_gt_param}},
#'   \code{\link{create_s_param}},
#'   \code{\link{create_scale_param}},
#'   and \code{\link{create_sigma_param}}
#' @return a parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create an alpha parameter
#' alpha_param <- create_alpha_param()
#'
#' # Use the parameter in a distribution
#' beta_distr <- create_beta_distr(
#'   alpha = alpha_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = beta_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @export
create_param <- function(
  name,
  id,
  value,
  ...
) {
  if (!beautier::is_param_name(name)) {
    parameters_as_string <- function() {
      s <- NULL
      for (p in beautier::get_param_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid parameter name, must be one these: ",
      parameters_as_string()
    )
  }
  parameter <- list(
    name = name,
    id = id,
    value = value,
    ...
  )
  parameter
}

#' Create a parameter called alpha
#' @inheritParams default_parameters_doc
#' @return a parameter called alpha
#' @note this parameter is used in a beta distribution
#'   (as returned by \code{\link{create_beta_distr}})
#' and gamma distribution
#'   (as returned by \code{\link{create_gamma_distr}})
#' and inverse-gamma distribution
#'   (as returned by \code{\link{create_inv_gamma_distr}}).
#' It cannot be estimated (as a hyper parameter) yet.
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' alpha_param <- create_alpha_param()
#'
#' # Use the parameter in a distribution
#' beta_distr <- create_beta_distr(
#'   alpha = alpha_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = beta_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_alpha_param create_param_alpha
#' @export create_alpha_param create_param_alpha
create_alpha_param <- create_param_alpha <- function(
  id = NA,
  value = 0.0
) {
  beautier::create_param(
    name = "alpha",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called beta
#' @inheritParams default_parameters_doc
#' @return a parameter called beta
#' @note this parameter is used in a beta distribution
#'   (as returned by \code{\link{create_beta_distr}})
#' and gamma distribution
#'   (as returned by \code{\link{create_gamma_distr}})
#' and inverse-gamma distribution
#'   (as returned by \code{\link{create_inv_gamma_distr}}).
#' It cannot be estimated (as a hyper parameter) yet.
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' beta_param <- create_beta_param()
#'
#' # Use the parameter in a distribution
#' gamma_distr <- create_gamma_distr(
#'   beta = beta_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = gamma_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_beta_param create_param_beta
#' @export create_beta_param create_param_beta
create_beta_param <- create_param_beta <- function(
  id = NA,
  value = 1.0
) {
  beautier::create_param(
    name = "beta",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called \code{clock_rate},
#'   as needed by \code{\link{create_strict_clock_model}}
#' @inheritParams default_parameters_doc
#' @return a parameter called rate
#' @note It cannot be estimated (as a hyper parameter) yet.
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' clock_rate_param <- create_clock_rate_param(
#'   id = "anthus_aco", value = 1.0
#' )
#'
#' # Use the parameter in a clock model
#' strict_clock_model <- create_strict_clock_model(
#'   clock_rate_param = clock_rate_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   clock_model = strict_clock_model
#' )
#' file.remove(beast2_input_file)
#' @aliases create_clock_rate_param create_param_clock_rate
#' @export create_clock_rate_param create_param_clock_rate
create_clock_rate_param <- create_param_clock_rate <- function(
  value = "1.0",
  id = NA
) {
  beautier::create_param(
    name = "clock_rate",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called kappa 1
#' @inheritParams default_parameters_doc
#' @return a parameter called kappa 1
#' @author Richèl J.C. Bilderbeek
#' @aliases create_kappa_1_param create_param_kappa_1
#' @export create_kappa_1_param create_param_kappa_1
create_kappa_1_param <- create_param_kappa_1 <- function(
  id = NA,
  lower = "0.0",
  value = "2.0",
  estimate = TRUE
) {
  beautier::create_param(
    name = "kappa_1",
    id = id,
    lower = lower,
    value = value,
    estimate = estimate
  )
}

#' Create a parameter called kappa 2
#' @inheritParams default_parameters_doc
#' @return a parameter called kappa 2
#' @author Richèl J.C. Bilderbeek
#' @aliases create_kappa_2_param create_param_kappa_2
#' @export create_kappa_2_param create_param_kappa_2
create_kappa_2_param <- create_param_kappa_2 <- function(
  id = NA,
  lower = "0.0",
  value = "2.0",
  estimate = TRUE
) {
  beautier::create_param(
    name = "kappa_2",
    id = id,
    lower = lower,
    value = value,
    estimate = estimate
  )
}

#' Create a parameter called lambda
#' @inheritParams default_parameters_doc
#' @return a parameter called lambda
#' @note this parameter is used in a Poisson distribution
#'   (as returned by \code{\link{create_poisson_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' lambda_param <- create_lambda_param()
#'
#' # Use the parameter in a distribution
#' poisson_distr <- create_poisson_distr(
#'   lambda = lambda_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = poisson_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_lambda_param create_param_lambda
#' @export create_lambda_param create_param_lambda
create_lambda_param <- create_param_lambda <- function(
  id = NA,
  value = 0.0
) {
  beautier::create_param(
    name = "lambda",
    id = id,
    value = value
  )
}

#' Create a parameter called m
#' @inheritParams default_parameters_doc
#' @return a parameter called m
#' @note this parameter is used in a log-normal distribution
#'   (as returned by \code{\link{create_log_normal_distr}})
#'   It cannot be estimated (as a hyper parameter) yet.
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' m_param <- create_m_param()
#'
#' # Use the parameter in a distribution
#' log_normal_distr <- create_log_normal_distr(
#'   m = m_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = log_normal_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_m_param create_param_m
#' @export create_m_param create_param_m
create_m_param <- create_param_m <- function(
  id = NA,
  value = 0.0
) {
  beautier::create_param(
    name = "m",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called mean
#' @inheritParams default_parameters_doc
#' @return a parameter called mean
#' @note this parameter is used in an exponential distribution
#'   (as returned by \code{\link{create_exp_distr}})
#'   and normal distribution
#'   (as returned by \code{\link{create_normal_distr}}).
#'   It cannot be estimated (as a hyper parameter) yet.
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' mean_param <- create_mean_param(value = 1.0)
#'
#' # Use the parameter in a distribution
#' exp_distr <- create_exp_distr(
#'   mean = mean_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = exp_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_mean_param create_param_mean
#' @export create_mean_param create_param_mean
create_mean_param <- create_param_mean <- function(
  id = NA,
  value = 0.0
) {
  beautier::create_param(
    name = "mean",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called mu
#' @inheritParams default_parameters_doc
#' @return a parameter called mu
#' @note this parameter is used in a Laplace distribution
#'   (as returned by \code{\link{create_laplace_distr}}).
#'   It cannot be estimated (as a hyper parameter) yet.
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' mu_param <- create_mu_param()
#'
#' # Use the parameter in a distribution
#' laplace_distr <- create_laplace_distr(
#'   mu = mu_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = laplace_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_mu_param create_param_mu
#' @export create_mu_param create_param_mu
create_mu_param <- create_param_mu <- function(
  id = NA,
  value = 0.0
) {
  beautier::create_param(
    name = "mu",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called 'rate AC'
#' @inheritParams default_parameters_doc
#' @return a parameter called 'rate AC'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create parameter
#' rate_ac_param <- create_rate_ac_param(value = 1, estimate = FALSE)
#'
#' # Use the parameter to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   site_model = create_gtr_site_model(
#'     rate_ac_param = rate_ac_param
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_rate_ac_param create_param_rate_ac
#' @export create_rate_ac_param create_param_rate_ac
create_rate_ac_param <- create_param_rate_ac <- function(
  id = NA,
  estimate = TRUE,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_ac",
    id = id,
    estimate = estimate,
    value = value,
    lower = lower
  )
}

#' Create a parameter called 'rate AG'
#' @inheritParams default_parameters_doc
#' @return a parameter called 'rate AG'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create parameter
#' rate_ag_param <- create_rate_ag_param(value = 1, estimate = FALSE)
#'
#' # Use the parameter to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   site_model = create_gtr_site_model(
#'     rate_ag_param = rate_ag_param
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_rate_ag_param create_param_rate_ag
#' @export create_rate_ag_param create_param_rate_ag
create_rate_ag_param <- create_param_rate_ag <- function(
  id = NA,
  estimate = TRUE,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_ag",
    id = id,
    estimate = estimate,
    value = value,
    lower = lower
  )
}

#' Create a parameter called 'rate AT'
#' @inheritParams default_parameters_doc
#' @return a parameter called 'rate AT'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create parameter
#' rate_at_param <- create_rate_at_param(value = 1, estimate = FALSE)
#'
#' # Use the parameter to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   site_model = create_gtr_site_model(
#'     rate_at_param = rate_at_param
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_rate_at_param create_param_rate_at
#' @export create_rate_at_param create_param_rate_at
create_rate_at_param <- create_param_rate_at <- function(
  id = NA,
  estimate = TRUE,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_at",
    id = id,
    estimate = estimate,
    value = value,
    lower = lower
  )
}

#' Create a parameter called 'rate CG'
#' @inheritParams default_parameters_doc
#' @return a parameter called 'rate CG'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create parameter
#' rate_cg_param <- create_rate_cg_param(value = 1, estimate = FALSE)
#'
#' # Use the parameter to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   site_model = create_gtr_site_model(
#'     rate_cg_param = rate_cg_param
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_rate_cg_param create_param_rate_cg
#' @export create_rate_cg_param create_param_rate_cg
create_rate_cg_param <- create_param_rate_cg <- function(
  id = NA,
  estimate = TRUE,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_cg",
    id = id,
    estimate = estimate,
    value = value,
    lower = lower
  )
}

#' Create a parameter called 'rate CT'
#' @inheritParams default_parameters_doc
#' @return a parameter called 'rate CT'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create parameter
#' rate_ct_param <- create_rate_ct_param(value = 1)
#'
#' # Use the parameter to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   site_model = create_gtr_site_model(
#'     rate_ct_param = rate_ct_param
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_rate_ct_param create_param_rate_ct
#' @export create_rate_ct_param create_param_rate_ct
create_rate_ct_param <- create_param_rate_ct <- function(
  id = NA,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_ct",
    id = id,
    estimate = FALSE,
    value = value,
    lower = lower
  )
}

#' Create a parameter called 'rate GT'
#' @inheritParams default_parameters_doc
#' @return a parameter called 'rate GT'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create parameter
#' rate_gt_param <- create_rate_gt_param(value = 1, estimate = FALSE)
#'
#' # Use the parameter to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   site_model = create_gtr_site_model(
#'     rate_gt_param = rate_gt_param
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_rate_gt_param create_param_rate_gt
#' @export create_rate_gt_param create_param_rate_gt
create_rate_gt_param <- create_param_rate_gt <- function(
  id = NA,
  estimate = TRUE,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_gt",
    id = id,
    estimate = estimate,
    lower = lower,
    value = value
  )
}

#' Create a parameter called s
#' @inheritParams default_parameters_doc
#' @return a parameter called s
#' @note this parameter is used in a log-normal distribution
#'   (as returned by \code{\link{create_log_normal_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' s_param <- create_s_param()
#'
#' # Use the parameter in a distribution
#' log_normal_distr <- create_log_normal_distr(
#'   s = s_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = log_normal_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_s_param create_param_s
#' @export create_s_param create_param_s
create_s_param <- create_param_s <- function(
  id = NA,
  value = 0.0,
  lower = 0.0,
  upper = Inf
) {
  beautier::create_param(
    name = "s",
    id = id,
    estimate = FALSE,
    value = value,
    lower = lower,
    upper = upper
  )
}

#' Create a parameter called scale
#' @inheritParams default_parameters_doc
#' @param value value of the parameter
#' @return a parameter called scale
#' @note this parameter is used in a Laplace distribution
#'   (as returned by \code{\link{create_laplace_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' scale_param <- create_scale_param()
#'
#' # Use the parameter in a distribution
#' laplace_distr <- create_laplace_distr(
#'   scale = scale_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = laplace_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_scale_param create_param_scale
#' @export create_scale_param create_param_scale
create_scale_param <- create_param_scale <- function(
  id = NA,
  value = 0.0
) {
  beautier::create_param(
    name = "scale",
    id = id,
    estimate = FALSE,
    value = value
  )
}

#' Create a parameter called sigma
#' @inheritParams default_parameters_doc
#' @param value value of the parameter
#' @return a parameter called sigma
#' @note this parameter is used in a normal distribution
#'   (as returned by \code{\link{create_normal_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create the parameter
#' sigma_param <- create_sigma_param()
#'
#' # Use the parameter in a distribution
#' normal_distr <- create_normal_distr(
#'   sigma = sigma_param
#' )
#'
#' # Use the distribution to create a BEAST2 input file
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   input_filename = get_fasta_filename(),
#'   beast2_input_file,
#'   tree_prior = create_yule_tree_prior(
#'     birth_rate_distr = normal_distr
#'   )
#' )
#' file.remove(beast2_input_file)
#' @aliases create_sigma_param create_param_sigma
#' @export create_sigma_param create_param_sigma
create_sigma_param <- create_param_sigma <- function(
  id = NA,
  value = 1.0
) {
  if (value <= 0.0) {
    stop("'value' must be non-zero and positive")
  }
  beautier::create_param(
    name = "sigma",
    id = id,
    estimate = FALSE,
    value = value
  )
}
