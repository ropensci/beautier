#' Read a tip dates file
#' @inheritParams default_params_doc
#' @return the tip dates as a tibble, with column names `taxon`
#' and `time`.  The `time` column creates strings instead
#' of numbers, to allow using the literally same values from the
#' tip dates file in the BEAST2 XML input file (i.e. avoid
#' rounding errors)
#' @author Richèl J.C. Bilderbeek
#' @examples
#' read_tipdates_file(get_beautier_path("babette_issue_109.tsv"))
#' @export
read_tipdates_file <- function(tipdates_filename) {
  check_tipdates_file(tipdates_filename)

  tipdates_table <- readr::read_tsv(
    tipdates_filename,
    col_names = c("taxon", "time"),
    col_types = readr::cols(
      taxon = readr::col_character(),
      time = readr::col_character() # Avoid rounding errors
    ),
    show_col_types = FALSE
  )
  testthat::expect_equal(ncol(tipdates_table), 2)
  testthat::expect_true(is.character(tipdates_table$taxon))
  testthat::expect_true(is.character(tipdates_table$time))
  tipdates_table
}
