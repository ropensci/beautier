#' Check if a \code{tracelog} is valid.
#'
#' Will call \link{stop} if not.
#' @inheritParams default_params_doc
#' @export
check_tracelog <- function(tracelog) {

  check_tracelog_names(tracelog)
  check_tracelog_values(tracelog)
}

#' Check if the \code{tracelog} has the list elements
#' of a valid \code{tracelog} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_tracelog} to create a valid \code{tracelog}
#' @author Richèl J.C. Bilderbeek
#' @noRd
check_tracelog_names <- function(tracelog) {

  list_element_names <- c(
    "filename", "log_every", "mode", "sanitise_headers", "sort"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(tracelog)) {
      stop(
        "'", arg_name, "' must be an element of an 'tracelog'. \n",
        "Tip: use 'create_tracelog'"
      )
    }
  }
}

#' Check if the tracelog has the list elements with valid values
#' for being a valid tracelog object.
#'
#' Calls \code{stop} if a value is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_tracelog} to create a valid tracelog
#' @author Richèl J.C. Bilderbeek
#' @noRd
check_tracelog_values <- function(tracelog) {

  if (!beautier::is_one_na(tracelog$filename)) {
    assertive::assert_is_character(tracelog$filename)
    assertive::assert_is_a_string(tracelog$filename)
  }
  assertive::assert_is_numeric(tracelog$log_every)
  assertive::assert_all_are_positive(tracelog$log_every)
  beautier::check_log_mode(tracelog$mode)
  assertive::assert_is_if_condition(tracelog$sanitise_headers)
  beautier::check_log_sort(tracelog$sort)
}
