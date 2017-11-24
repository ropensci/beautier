#' General function to create a parameter.
#' @param name the parameters' name. Valid
#'   names can be found in \code{\link{get_param_names}}
#' @param id the parameter's ID
#' @param ... specific parameter parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_alphaparam}},
#'   \code{\link{create_betaparam}},
#'   \code{\link{create_clock_rateparam}},
#'   \code{\link{create_mparam}},
#'   \code{\link{create_meanparam}},
#'   \code{\link{create_muparam}},
#'   \code{\link{create_sparam}},
#'   \code{\link{create_scaleparam}},
#'   and \code{\link{create_sigmaparam}}
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
#' @export
create_alphaparam <- function(
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
create_betaparam <- function(
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
create_mparam <- function(
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
create_meanparam <- function(
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
#'   muparam <- create_muparam()
#'   testit::assert(is_muparam(muparam))
#' @export
create_muparam <- function(
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
#'   clock_rateparam <- create_clock_rateparam(
#'     id = "anthus_aco", estimate = FALSE, value = 1.0
#'   )
#'   testit::assert(is_clock_rateparam(clock_rateparam))
#' @export
create_clock_rateparam <- function(
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
create_sparam <- function(
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
#'   scaleparam <- create_scaleparam()
#'   testit::assert(is_scaleparam(scaleparam))
#' @export
create_scaleparam <- function(
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
create_sigmaparam <- function(
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
