#' Get the alignment ID from one or more FASTA filenames.
#'
#' This is done in the same way as BEAST2 does by default
#' @inheritParams default_params_doc
#' @return the IDs from one or more FASTA files
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   created <- get_alignment_ids(
#'     get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
#'   )
#'   expected <- c(
#'     get_alignment_id(get_beautier_path("anthus_aco.fas")),
#'     get_alignment_id(get_beautier_path("anthus_nd2.fas"))
#'   )
#'   testit::assert(created == expected)
#' @export
get_alignment_ids <- function(fasta_filenames) {
  ids <- fasta_filenames
  unlist(lapply(ids, get_alignment_id)) # nolint beautier function
}
