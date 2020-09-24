#' Check if the parameter is a valid parameter
#'
#' Calls \code{stop} if the parameter is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_param} to create a valid parameter
#' @examples
#' check_param(create_alpha_param())
#' check_param(create_beta_param())
#' @author Richèl J.C. Bilderbeek
#' @export
check_param <- function(param) {
  beautier::check_param_names(param)
  beautier::check_param_types(param)
}

#' Check if the \code{param} has the list elements
#' of a valid \code{param} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_param} to create a valid \code{param}
#' @author Richèl J.C. Bilderbeek
#' @export
check_param_names <- function(param) {
  list_element_names <- c(
    "name", "id", "value"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(param)) {
      stop(
        "'", arg_name, "' must be an element of an 'param'. \n",
        "Tip: use 'create_param'"
      )
    }
  }
}

#' Check if the \code{param} has the list elements
#' of the right type for a valid \code{param} object.
#'
#' Calls \code{stop} if an element has the incorrect type
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_param} to create a valid \code{param}
#' @author Richèl J.C. Bilderbeek
#' @export
check_param_types <- function(param) {

  if (!param$name %in% beautier::get_param_names()) {
    stop("'param$name' must be a valid parameter name")
  }
  if (beautier::is_one_na(param$value)) {
    stop("'param$value' must not be NA")
  }
}
