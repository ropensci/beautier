#' Get the alignment ID from one or more FASTA filenames
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
get_alignment_ids <- function(fasta_filenames) {
  ids <- fasta_filenames
  unlist(lapply(ids, get_alignment_id))
}
