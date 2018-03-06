#' Conclude the IDs from one or more FASTA filenames
#' @inheritParams default_params_doc
#' @param capitalize_first_char_id capitalize the first character of the IDs
#' @return the IDs
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{get_id}} for one filename
#' @examples
#'   # Basic usage
#'   testit::assert(get_ids(c("a.fas", "b.fas")) == c("a", "b"))
#'
#'   # Usage to create a BEAST2 XML file
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'   clock_models <- create_strict_clock_models(
#'     ids = get_ids(fasta_filenames)
#'   )
#'
#'   create_beast2_input_file(
#'     fasta_filenames,
#'     "create_strict_clock_models.xml",
#'     clock_models = clock_models
#'   )
#'   testit::assert(file.exists("create_strict_clock_models.xml"))
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
