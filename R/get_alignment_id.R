#' Conclude the ID from a FASTA filename.
#'
#' This is done in the same way as BEAST2 will do so.
#' @inheritParams default_params_doc
#' @param capitalize_first_char_id if TRUE, the first character will
#'   be capitalized
#' @return an alignment's ID
#' @examples
#'   # Path need not exist, use UNIX path as example
#'   created <- get_alignment_id("/home/homer/anthus_aco_sub.fas")
#'   expected <- "anthus_aco_sub"
#'   testit::assert(created == expected)
#' @author Richèl J.C. Bilderbeek
#' @export
get_alignment_id <- function(
  fasta_filename,
  capitalize_first_char_id = FALSE
) {
  id <- get_file_base_sans_ext(fasta_filename) # nolint beautier function
  if (capitalize_first_char_id == TRUE) {
    id <- paste0(toupper(substr(id, 1, 1)), substring(id, 2))
  }
  id
}
