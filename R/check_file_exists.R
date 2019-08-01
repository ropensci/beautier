#' Function to check if a file exists.
#' Calls \code{stop} if the file is absent
#' @param filename name of the file
#' @param filename_description description of the filename
#' @return nothing. Will \code{stop} if the file is absent,
#'   with a proper error message
#' @examples
#' library(testthat)
#'
#' expect_silent(
#'   check_file_exists(
#'     get_beautier_path("anthus_aco_sub.fas")
#'   )
#' )
#'
#' # Minimal use
#' expect_error(
#'   check_file_exists("absent"),
#'   "File not found. Could not find file with path 'absent'"
#' )
#'
#' # Add a description
#' absent_filename <- "absent"
#' expect_error(
#'   check_file_exists(absent_filename, "absent_filename"),
#'   "File 'absent_filename' not found. Could not find file with path 'absent'"
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
check_file_exists <- function(
  filename,
  filename_description = NA
) {
  if (!file.exists(filename)) {
    if (is_one_na(filename_description)) {
      stop(
        "File not found. ",
        "Could not find file with path '",
        filename,
        "'"
      )
    } else {
      assertive::assert_is_a_string(filename_description)
      stop(
        "File '", filename_description, "' not found. ",
        "Could not find file with path '",
        filename,
        "'"
      )
    }
  }
}
