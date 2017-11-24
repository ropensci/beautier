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
#'   \code{\link{create_m_param}},
#'   \code{\link{create_mean_param}},
#'   \code{\link{create_mu_param}},
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
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   alpha_param <- create_alpha_param()
#'   testit::assert(is_alpha_param(alpha_param))
#'
#'   beta_distr <- create_beta_distr(
#'     alpha = alpha_param
#'   )
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
create_alpha_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "alpha",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called beta
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called beta
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_beta_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "beta",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called lambda
#' @inheritParams create_param
#' @param value value of the parameter
#' @return a parameter called lambda
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_lambdaparam <- function(
  id = NA,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "lambda",
      id = id,
      value = value
    )
  )
}

#' Create a parameter called m
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called m
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_m_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "m",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called mean
#' @inheritParams create_param
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called mean
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_mean_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "mean",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called mu
#' @inheritParams create_param
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called mu
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   muparam <- create_mu_param()
#'   testit::assert(is_mu_param(muparam))
#' @export
create_mu_param <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_param(
      name = "mu",
      id = id,
      estimate = estimate,
      value = value
    )
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
#'   clock_rateparam <- create_clock_rate_param(
#'     id = "anthus_aco", estimate = FALSE, value = 1.0
#'   )
#'   testit::assert(is_clock_rate_param(clock_rateparam))
#' @export
create_clock_rate_param <- function(
  id = NA,
  estimate = FALSE,
  value = "1.0"
) {
  return(
    beautier::create_param(
      name = "clock_rate",
      id = id,
      estimate = estimate,
      value = value
    )
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
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
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
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   scaleparam <- create_scale_param()
#'   testit::assert(is_scale_param(scaleparam))
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
#' @seealso the function \code{\link{create_param}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
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
