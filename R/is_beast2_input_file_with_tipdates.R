#' Determine if the file is a BEAST2 input file that has tip dating
#' @inheritParams default_params_doc
#' @return TRUE if x is the BEAST2 input file has tip dating, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_beast2_input_file_with_tipdates(
#'   get_beautier_path("babette_issue_109_expected_v2_6.xml")
#' ) # TRUE
#'
#' check_empty_beautier_folder()
#' @export
is_beast2_input_file_with_tipdates <- function(beast2_input_file) {
  text <- readr::read_lines(beast2_input_file)
  tipdate_regex <- paste0(
    "<trait id=\"dateTrait.t:.*\" ",
    "spec=\"beast.evolution.tree.TraitSet\" ",
    "traitname=\"date(-forward)?\" ",
    "value=\".+\">"
  )
  n_hits <- sum(stringr::str_count(text, tipdate_regex))

  testthat::expect_true(n_hits == 0 || n_hits == 1)
  n_hits == 1
}
