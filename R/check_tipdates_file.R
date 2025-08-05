#' Check if the tip dates file is valid
#' @inheritParams default_params_doc
#' @return nothing
#' @examples
#' check_empty_beautier_folder()
#'
#' check_tipdates_file(
#'   get_beautier_path("babette_issue_109.tsv")
#' ) # OK
#'
#' try(
#'   check_tipdates_file(
#'     get_beautier_path("babette_issue_109_no_tabs.tsv")
#'   ) # ERROR
#' )
#'
#' try(
#'   check_tipdates_file(
#'     get_beautier_path("babette_issue_109_with_header.tsv")
#'   ) # ERROR
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_tipdates_file <- function(tipdates_filename) {
  if (!file.exists(tipdates_filename)) {
    stop(
      "Tipdating filename not found at path '",
      tipdates_filename, "'. \n",
      "Tip: set either to NA (i.e. no tip dating) ",
      "or to a valid path"
    )
  }

  tipdates_text <- readr::read_lines(tipdates_filename)

  # Remove empty lines
  tipdates_text <- tipdates_text[tipdates_text != ""]

  # Must be tab-seperated
  if (!all(stringr::str_detect(tipdates_text, "\\t"))) {
    stop(
      "Tipdating filename at path '",
      tipdates_filename, "' ",
      "is not a tab-separated file. \n",
      "Tip: edit the file to have tabs as a column separator and try again"
    )
  }

  # First line must not be a header
  first_line <- tipdates_text[1]
  second_value_as_text <- stringr::str_split(first_line, "\\t")[[1]][2]
  second_value <- suppressWarnings(as.numeric(second_value_as_text))

  if (beautier::is_one_na(second_value)) {
    stop(
      "Tipdating filename at path '",
      tipdates_filename, "' ",
      "has a header. \n",
      "Tip: remove the first line of this file and try again"
    )
  }
}
