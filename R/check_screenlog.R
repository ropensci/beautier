#' Check if a \code{screenlog} is valid.
#'
#' Will call \link{stop} if not.
#' @inheritParams default_params_doc
#' @examples
#' check_empty_beautier_folder()
#'
#' check_screenlog(create_test_screenlog())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_screenlog <- function(screenlog) {

  beautier::check_screenlog_names(screenlog)
  beautier::check_screenlog_values(screenlog)
}

#' Check if the \code{screenlog} has the list elements
#' of a valid \code{screenlog} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_screenlog} to create a valid \code{screenlog}
#' @author Richèl J.C. Bilderbeek
#' @export
check_screenlog_names <- function(screenlog) {

  list_element_names <- c(
    "filename", "log_every", "mode", "sanitise_headers", "sort"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(screenlog)) {
      stop(
        "'", arg_name, "' must be an element of an 'screenlog'. \n",
        "Tip: use 'create_screenlog'"
      )
    }
  }
}

#' Check if the screenlog has the list elements with valid values
#' for being a valid screenlog object.
#'
#' Calls \code{stop} if a value is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_screenlog} to create a valid screenlog
#' @author Richèl J.C. Bilderbeek
#' @export
check_screenlog_values <- function(screenlog) {
  beautier::check_filename(
    screenlog$filename,
    allow_empty_str = TRUE,
    allow_na = TRUE
  )
  assertive::assert_is_numeric(screenlog$log_every)
  assertive::assert_all_are_positive(screenlog$log_every)
  beautier::check_log_mode(screenlog$mode)
  assertive::assert_is_if_condition(screenlog$sanitise_headers)
  beautier::check_log_sort(screenlog$sort)
}
