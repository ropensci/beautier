#' Checks if all filenames have a FASTA filename extension
#' @param filenames filenames
#' @return TRUE if all filenames have a FASTA filename extension
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' are_fasta_filenames("1.fas")
#' are_fasta_filenames("1.fasta")
#' are_fasta_filenames("1.FAS")
#' are_fasta_filenames("1.FASTA")
#' are_fasta_filenames(c("1.fas", "2.fas"))
#'
#' # FALSE
#' are_fasta_filenames("")
#' are_fasta_filenames(NA)
#' are_fasta_filenames(NULL)
#' are_fasta_filenames(Inf)
#' are_fasta_filenames("1.fasX")
#' are_fasta_filenames(c("1.fas", "2.exe"))
#' are_fasta_filenames(c("1.bat", "2.exe"))
#'
#' check_empty_beautier_folder()
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
