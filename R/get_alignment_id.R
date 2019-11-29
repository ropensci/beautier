#' Conclude the ID from a FASTA filename.
#'
#' This is done in the same way as BEAST2 will do so.
#' @inheritParams default_params_doc
#' @param capitalize_first_char_id if TRUE, the first character will
#'   be capitalized
#' @return an alignment's ID
#' @seealso Use \link{check_alignment_id} to check if an alignment
#' ID is valid.
#' @examples
#' library(testthat)
#'
#' # Path need not exist, use UNIX path as example
#' created <- get_alignment_id("/home/homer/anthus_aco_sub.fas")
#' expected <- "anthus_aco_sub"
#' expect_equal(created, expected)
#' expect_silent(check_alignment_id(created))
#' @author Richèl J.C. Bilderbeek
#' @export
get_alignment_id <- function(
  fasta_filename,
  capitalize_first_char_id = FALSE
) {
  id <- beautier::get_file_base_sans_ext(fasta_filename)
  if (capitalize_first_char_id == TRUE) {
    id <- paste0(toupper(substr(id, 1, 1)), substring(id, 2))
  }
  beautier::check_alignment_id(id)
  id
}
