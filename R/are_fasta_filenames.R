#' Checks if all filenames have a FASTA filename extension
#' @param filenames filenames
#' @return TRUE if all filenames have a FASTA filename extension
#' @examples
#' library(testthat)
#'
#' expect_true(are_fasta_filenames("1.fas"))
#' expect_true(are_fasta_filenames("1.fasta"))
#' expect_true(are_fasta_filenames("1.FAS"))
#' expect_true(are_fasta_filenames("1.FASTA"))
#' expect_true(are_fasta_filenames(c("1.fas", "2.fas")))
#'
#' expect_false(are_fasta_filenames(""))
#' expect_false(are_fasta_filenames(NA))
#' expect_false(are_fasta_filenames(NULL))
#' expect_false(are_fasta_filenames(Inf))
#' expect_false(are_fasta_filenames("1.fasX"))
#'
#' expect_false(are_fasta_filenames(c("1.fas", "2.exe")))
#' expect_false(are_fasta_filenames(c("1.bat", "2.exe")))
#' @author Richèl J.C. Bilderbeek
#' @export
are_fasta_filenames <- function(filenames) {
  matches <- stringr::str_match(
    string = filenames,
    pattern = ".*\\.(fas|fasta|FAS|FASTA)$"
  )[, 1]
  if (length(matches) == 0) return(FALSE)
  all(!is.na(matches))
}
