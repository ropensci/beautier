#' Conclude the ID from a FASTA filename
#' @param fasta_filename name of a FASTA file
#' @param capitalize_first_char_id capitalize the first character of the ID
#' @return the ID
#' @examples
#'   testit::assert(get_id("anthus_aco.fas") == "anthus_aco")
#'   testit::assert(
#'     get_id("anthus_aco.fas", capitalize_first_char_id = TRUE)
#'     == "Anthus_aco")
#' @seealso Use \code{\link{get_ids}} for one or more filenames
#' @author Richel J.C. Bilderbeek
#' @export
get_id <- function(
  fasta_filename,
  capitalize_first_char_id = FALSE
) {
  id <- get_file_base_sans_ext(fasta_filename) # nolint internal function
  if (capitalize_first_char_id == TRUE) {
    id <- paste0(toupper(substr(id, 1, 1)), substring(id, 2))
  }
  id
}
