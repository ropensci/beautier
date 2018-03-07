#' Get the alignment ID from a FASTA filename
#' @inheritParams default_params_doc
#' @examples
#'   created <- get_alignment_id("/home/homer/anthus_aco_sub.fas")
#'   expected <- "anthus_aco_sub"
#'   testit::assert(created == expected)
#' @export
#' @author Richel J.C. Bilderbeek
get_alignment_id <- function(fasta_filename) {
  get_file_base_sans_ext(fasta_filename)
}
