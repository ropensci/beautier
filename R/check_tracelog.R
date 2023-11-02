#' Check if a \code{tracelog} is valid.
#'
#' Will call \link{stop} if not.
#' @inheritParams default_params_doc
#' @return No return value, called for side effects
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
#' @export
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
#' @export
check_tracelog_values <- function(tracelog) {

  check_filename(tracelog$filename, allow_na = TRUE)
  lapply(tracelog$log_every, function(x) check_number_whole(x, min = 1, arg = "log_every"))

  check_log_mode(tracelog$mode)
  check_logical(tracelog$sanitise_headers)
  check_log_sort(tracelog$sort)
  invisible(tracelog)
}
