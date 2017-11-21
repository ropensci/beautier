#' Conclude the IDs from one or more FASTA filenames
#' @param fasta_filenames one or more FASTA filenames
#' @param capitalize_first_char_id capitalize the first character of the IDs
#' @return the IDs
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{get_id}} for one filename
#' testit::assert(get_ids("a.fas", "b.fas") == c("a", "b"))
#' testit::assert(
#'   get_ids(
#'     c("a.fas", "b.fas"),
#'     capitalize_first_char_id = TRUE
#'   ) == c("A", "B")
#' )
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
