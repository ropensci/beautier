#' Check if the `filename` is valid
#'
#' Calls \code{stop} if the filename is invalid
#' @inheritParams default_params_doc
#' @return nothing
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
  testthat::expect_equal(1, length(allow_empty_str))
  testthat::expect_equal(1, length(allow_na))
  testthat::expect_true(allow_empty_str == TRUE || allow_empty_str == FALSE)
  testthat::expect_true(allow_na == TRUE || allow_na == FALSE)

  testthat::expect_equal(length(filename), 1)
  if (allow_na && is.na(filename)) return(invisible(filename))
  testthat::expect_true(is.character(filename))
  if (stringr::str_detect(filename, " ")) {
    stop(
      "Filenames must not have space. \n",
      "filename: ", filename
    )
  }
  if (!allow_empty_str) {
    testthat::expect_true(nchar(filename) > 0)
  }
  invisible(filename)
}
