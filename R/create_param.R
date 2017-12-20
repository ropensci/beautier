#' General function to create a parameter.
#' @param name the parameters' name. Valid
#'   names can be found in \code{\link{get_param_names}}
#' @param id the parameter's ID
#' @param ... specific parameter parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_alpha_param}},
#'   \code{\link{create_beta_param}},
#'   \code{\link{create_clock_rate_param}},
#'   \code{\link{create_kappa_1_param}},
#'   \code{\link{create_kappa_2_param}},
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
#' @author Richel J.C. Bilderbeek
#' @export
create_param <- function(
  name,
  id,
  ...
) {
  if (!is_param_name(name)) {
    parameters_as_string <- function() {
      s <- NULL
      for (p in get_param_names()) {
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
    ...
  )
  parameter
}

#' Create a parameter called alpha
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called alpha
#' @note this parameter is used in a beta distribution
#'   (as returned by \code{\link{create_beta_distr}})
#' and gamma distribution
#'   (as returned by \code{\link{create_gamma_distr}})
#' and inverse gamma distribution
#'   (as returned by \code{\link{create_inv_gamma_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   alpha_param <- create_alpha_param()
#'
#'   # Use the parameter in a distribution
#'   beta_distr <- create_beta_distr(
#'     alpha = alpha_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = beta_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_alpha_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  beautier::create_param(
    name = "alpha",
    id = id,
    estimate = estimate,
    value = value
  )
}

#' Create a parameter called beta
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called beta
#' @note this parameter is used in a beta distribution
#'   (as returned by \code{\link{create_beta_distr}})
#' and gamma distribution
#'   (as returned by \code{\link{create_gamma_distr}})
#' and inverse gamma distribution
#'   (as returned by \code{\link{create_inv_gamma_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   beta_param <- create_beta_param()
#'
#'   # Use the parameter in a distribution
#'   gamma_distr <- create_gamma_distr(
#'     beta = beta_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = gamma_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_beta_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  beautier::create_param(
    name = "beta",
    id = id,
    estimate = estimate,
    value = value
  )
}

#' Create a parameter called \code{clock_rate},
#'   as needed by \code{\link{create_strict_clock_model}}
#' @param id the alignment id
#' @param estimate TRUE if this parameter is estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called rate
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   clock_rate_param <- create_clock_rate_param(
#'     id = "anthus_aco", estimate = FALSE, value = 1.0
#'   )
#'
#'   # Use the parameter in a clock model
#'   strict_clock_model <- create_strict_clock_model(
#'     clock_rate_param = clock_rate_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     clock_models = strict_clock_model
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_clock_rate_param <- function(
  value = "1.0",
  estimate = FALSE,
  id = NA
) {
  beautier::create_param(
    name = "clock_rate",
    id = id,
    estimate = estimate,
    value = value
  )
}

#' Create a parameter called kappa 1
#' @inheritParams create_param
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called kappa 1
#' @author Richel J.C. Bilderbeek
#' @export
create_kappa_1_param <- function(
  id = NA,
  lower = "0.0",
  value = "2.0"
) {
  beautier::create_param(
    name = "kappa_1",
    id = id,
    lower = lower,
    value = value
  )
}

#' Create a parameter called kappa 2
#' @inheritParams create_param
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called kappa 2
#' @author Richel J.C. Bilderbeek
#' @export
create_kappa_2_param <- function(
  id = NA,
  lower = "0.0",
  value = "2.0"
) {
  beautier::create_param(
    name = "kappa_2",
    id = id,
    lower = lower,
    value = value
  )
}
#' Create a parameter called lambda
#' @inheritParams create_param
#' @param value value of the parameter
#' @return a parameter called lambda
#' @note this parameter is used in a Poisson distribution
#'   (as returned by \code{\link{create_poisson_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   lambda_param <- create_lambda_param()
#'
#'   # Use the parameter in a distribution
#'   poisson_distr <- create_poisson_distr(
#'     lambda = lambda_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = poisson_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_lambda_param <- function(
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
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called m
#' @note this parameter is used in a log-normal distribution
#'   (as returned by \code{\link{create_log_normal_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   m_param <- create_m_param()
#'
#'   # Use the parameter in a distribution
#'   log_normal_distr <- create_log_normal_distr(
#'     m = m_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = log_normal_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_m_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  beautier::create_param(
    name = "m",
    id = id,
    estimate = estimate,
    value = value
  )
}

#' Create a parameter called mean
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called mean
#' @note this parameter is used in an exponential distribution
#'   (as returned by \code{\link{create_exp_distr}})
#' and normal distribution
#'   (as returned by \code{\link{create_normal_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   mean_param <- create_mean_param()
#'
#'   # Use the parameter in a distribution
#'   exp_distr <- create_exp_distr(
#'     mean = mean_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = exp_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_mean_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  beautier::create_param(
    name = "mean",
    id = id,
    estimate = estimate,
    value = value
  )
}

#' Create a parameter called mu
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called mu
#' @note this parameter is used in a Laplace distribution
#'   (as returned by \code{\link{create_laplace_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   mu_param <- create_mu_param()
#'
#'   # Use the parameter in a distribution
#'   laplace_distr <- create_laplace_distr(
#'     mu = mu_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = laplace_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_mu_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  beautier::create_param(
    name = "mu",
    id = id,
    estimate = estimate,
    value = value
  )
}

#' Create a parameter called 'rate AC'
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called 'rate AC'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_ac_param <- create_rate_ac_param()
#'   # TODO
#' @export
create_rate_ac_param <- function(
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
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called 'rate AG'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_ag_param <- create_rate_ag_param()
#'   # TODO
#' @export
create_rate_ag_param <- function(
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
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called 'rate AT'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_at_param <- create_rate_at_param()
#'   # TODO
#' @export
create_rate_at_param <- function(
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
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called 'rate CG'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_cg_param <- create_rate_cg_param()
#'   # TODO
#' @export
create_rate_cg_param <- function(
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
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called 'rate CT'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_ct_param <- create_rate_ct_param()
#'   # TODO
#' @export
create_rate_ct_param <- function(
  id = NA,
  estimate = TRUE,
  value = "1.0",
  lower = "0.0"
) {
  beautier::create_param(
    name = "rate_ct",
    id = id,
    estimate = estimate,
    value = value,
    lower = lower
  )
}

#' Create a parameter called 'rate GT'
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lowest possible value of the parameter
#' @return a parameter called 'rate GT'
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_gt_param <- create_rate_gt_param()
#'   # TODO
#' @export
create_rate_gt_param <- function(
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
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lower value of the parameter
#' @param upper upper value of the parameter
#' @return a parameter called s
#' @note this parameter is used in a log-normal distribution
#'   (as returned by \code{\link{create_log_normal_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   s_param <- create_s_param()
#'
#'   # Use the parameter in a distribution
#'   log_normal_distr <- create_log_normal_distr(
#'     s = s_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = log_normal_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_s_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0,
  lower = 0.0,
  upper = 0.0
) {
  return(
    beautier::create_param(
      name = "s",
      id = id,
      estimate = estimate,
      value = value,
      lower = lower,
      upper = upper
    )
  )
}

#' Create a parameter called scale
#' @inheritParams create_param
#' @param estimate TRUE if this parameter scale be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called scale
#' @note this parameter is used in a Laplace distribution
#'   (as returned by \code{\link{create_laplace_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   scale_param <- create_scale_param()
#'
#'   # Use the parameter in a distribution
#'   laplace_distr <- create_laplace_distr(
#'     scale = scale_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = laplace_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_scale_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "scale",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called sigma
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called sigma
#' @note this parameter is used in a normal distribution
#'   (as returned by \code{\link{create_normal_distr}})
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create the parameter
#'   sigma_param <- create_sigma_param()
#'
#'   # Use the parameter in a distribution
#'   normal_distr <- create_normal_distr(
#'     sigma = sigma_param
#'   )
#'
#'   # Use the distribution to create a BEAST2 input file
#'   input_fasta_filename <- beautier::get_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = normal_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
#' @export
create_sigma_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "sigma",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}
