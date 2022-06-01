#' Check if a \code{treelog} is valid.
#'
#' Will call \link{stop} if not.
#' @inheritParams default_params_doc
#' @examples
#' check_empty_beautier_folder()
#'
#' check_treelog(create_test_treelog())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_treelog <- function(treelog) {

  check_treelog_names(treelog)
  check_treelog_values(treelog)
}

#' Check if the \code{treelog} has the list elements
#' of a valid \code{treelog} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_treelog} to create a valid \code{treelog}
#' @author Richèl J.C. Bilderbeek
#' @export
check_treelog_names <- function(treelog) {

  list_element_names <- c(
    "filename", "log_every", "mode", "sanitise_headers", "sort"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(treelog)) {
      stop(
        "'", arg_name, "' must be an element of an 'treelog'. \n",
        "Tip: use 'create_treelog'"
      )
    }
  }
}

#' Check if the treelog has the list elements with valid values
#' for being a valid treelog object.
#'
#' Calls \code{stop} if a value is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_treelog} to create a valid treelog
#' @author Richèl J.C. Bilderbeek
#' @export
check_treelog_values <- function(treelog) {
  beautier::check_filename(filename = treelog$filename, allow_na = TRUE)
  assertive::assert_is_numeric(treelog$log_every)
  assertive::assert_all_are_positive(treelog$log_every)
  beautier::check_log_mode(treelog$mode)
  assertive::assert_is_if_condition(treelog$sanitise_headers)
  beautier::check_log_sort(treelog$sort)
}
