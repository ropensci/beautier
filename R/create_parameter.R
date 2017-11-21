#' General function to create a parameter.
#' @param name the parameters' name. Valid
#'   names can be found in \code{\link{get_parameter_names}}
#' @param id the parameter's ID
#' @param ... specific parameter parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_alpha_parameter}},
#'   \code{\link{create_beta_parameter}},
#'   \code{\link{create_clock_rate_parameter}},
#'   \code{\link{create_m_parameter}},
#'   \code{\link{create_mean_parameter}},
#'   \code{\link{create_mu_parameter}},
#'   \code{\link{create_s_parameter}},
#'   \code{\link{create_scale_parameter}},
#'   and \code{\link{create_sigma_parameter}}
#' @return a parameter
#' @author Richel J.C. Bilderbeek
#' @export
create_parameter <- function(
  name,
  id,
  ...
) {
  if (!is_parameter_name(name)) {
    parameters_as_string <- function() {
      s <- NULL
      for (p in get_parameter_names()) {
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
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called alpha
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_alpha_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "alpha",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called beta
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called beta
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_beta_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "beta",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called lambda
#' @inheritParams create_parameter
#' @param value value of the parameter
#' @return a parameter called lambda
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_lambda_parameter <- function(
  id = NA,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "lambda",
      id = id,
      value = value
    )
  )
}

#' Create a parameter called m
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called m
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_m_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "m",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called mean
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called mean
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_mean_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "mean",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called mu
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter mu be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called mu
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_mu_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
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
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @examples
#'   clock_rate_parameter <- create_clock_rate_parameter(
#'     id = "anthus_aco", estimate = FALSE, value = 1.0
#'   )
#'   testit::assert(is_clock_rate_parameter(clock_rate_parameter))
#' @export
create_clock_rate_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = "1.0"
) {
  return(
    beautier::create_parameter(
      name = "clock_rate",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called s
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @param lower lower value of the parameter
#' @param upper upper value of the parameter
#' @return a parameter called s
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_s_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0,
  lower = 0.0,
  upper = 0.0
) {
  return(
    beautier::create_parameter(
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
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter scale be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called scale
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_scale_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "scale",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}

#' Create a parameter called sigma
#' @inheritParams create_parameter
#' @param estimate TRUE if this parameter alpha be estimated by BEAST2,
#'   FALSE otherwise
#' @param value value of the parameter
#' @return a parameter called sigma
#' @seealso the function \code{\link{create_parameter}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_sigma_parameter <- function(
  id = NA,
  estimate = FALSE,
  value = 0.0
) {
  return(
    beautier::create_parameter(
      name = "sigma",
      id = id,
      estimate = estimate,
      value = value
    )
  )
}
