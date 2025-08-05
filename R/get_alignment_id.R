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
#' check_empty_beautier_folder()
#'
#' # Path need not exist, use UNIX path as example
#' # anthus_aco_sub
#' alignment_id <- get_alignment_id("/home/homer/anthus_aco_sub.fas")
#' check_alignment_id(alignment_id)
#'
#' check_empty_beautier_folder()
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
