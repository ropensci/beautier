#' General function to create a parameter.
#' @param name the parameters' name. Valid
#'   names can be found in \code{\link{get_parameter_names}}
#' @param id the parameter's ID
#' @param ... specific parameter parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_parameter_alpha}},
#'   and \code{\link{create_parameter_beta}}
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
#' @seealso the function \code{\link{create_parameter_alpha}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_parameter_alpha <- function(
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
#' @seealso the function \code{\link{create_parameter_alpha}} contains a list
#'   of all parameters that can be created
#' @author Richel J.C. Bilderbeek
#' @export
create_parameter_beta <- function(
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
