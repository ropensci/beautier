#' Check if the site model is a valid site model
#'
#' Calls \code{stop} if the site models are invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_site_model} to create a valid site model
#' @examples
#' check_empty_beautier_folder()
#'
#' check_site_model(create_jc69_site_model())
#' check_site_model(create_hky_site_model())
#' check_site_model(create_tn93_site_model())
#' check_site_model(create_gtr_site_model())
#'
#' # Can use list of one site model
#' check_site_model(list(create_jc69_site_model()))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_site_model <- function(site_model) {
  if (length(site_model) == 1) {
    check_site_model_names(site_model[[1]])
    check_site_model_types(site_model[[1]])
    return()
  }
  check_site_model_names(site_model)
  check_site_model_types(site_model)
}

#' Check if the \code{site_model} has the list elements
#' of a valid \code{site_model} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_site_model} to create a valid \code{site_model}
#' @author Richèl J.C. Bilderbeek
#' @export
check_site_model_names <- function(site_model) {

  list_element_names <- c(
    "name", "id", "gamma_site_model"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(site_model)) {
      stop(
        "'", arg_name, "' must be an element of an 'site_model'. \n",
        "Tip: use 'create_site_model'"
      )
    }
  }
}

#' Check if the \code{site_model} has the list elements
#' of the right type for a valid \code{site_model} object.
#'
#' Calls \code{stop} if an element has the incorrect type
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_site_model} to create a valid \code{site_model}
#' @author Richèl J.C. Bilderbeek
#' @export
check_site_model_types <- function(site_model) {

  if (!is_site_model_name(site_model$name)) {
    stop("Invalid site model name. Actual value: '", site_model$name, "'")
  }
  if (!is_gamma_site_model(site_model$gamma_site_model)) {
    stop("Invalid 'site_model$gamma_site_model'")
  }
}
