#' Conclude the IDs from one or more FASTA filenames
#' @param fasta_filenames one or more FASTA filenames
#' @param capitalize_first_char_id capitalize the first character of the IDs
#' @return the ID
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{get_id}} for one filename
#' @export
get_ids <- function(
  fasta_filenames,
  capitalize_first_char_id = FALSE
) {
  ids <- fasta_filenames
  for (i in seq_along(ids)) {
    ids[i] <- get_id(fasta_filenames[i],
      capitalize_first_char_id = capitalize_first_char_id)
  }
  ids
}
