#' Check if the `filename` is valid
#'
#' Calls \code{stop} if the filename is invalid
#' @inheritParams default_params_doc
#' @return The filename (invisibly)
#' @examples
#' check_empty_beautier_folder()
#'
#' check_filename("trace.log")
#' check_filename("my.trees")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_filename <- function(
  filename,
  allow_empty_str = FALSE,
  allow_na = FALSE
) {
  check_logical(allow_empty_str)
  check_logical(allow_na)
  check_string(filename, allow_na = allow_na, allow_empty = allow_empty_str)

  if (!is.na(filename) && nzchar(filename) && stringr::str_detect(filename, " ")) {
    stop(
      "Filenames must not have space. \n",
      "filename: ", filename
    )
  }
  invisible(filename)
}
