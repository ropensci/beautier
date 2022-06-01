#' Extract the number of taxa from a file
#' @param filename name of a FASTA file
#' @return the number of taxa
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' fasta_filename <- get_beautier_path("test_output_5.fas")
#' # 5
#' get_n_taxa(fasta_filename)
#'
#' check_empty_beautier_folder()
#' @export
get_n_taxa <- function(filename) {
  assertive::assert_is_a_string(filename)
  assertive::assert_all_are_existing_files(filename)
  tryCatch({
      return(length(seqinr::read.fasta(filename)))
    },
    error = function(e) {
      stop(
        "'filename' must be a valid FASTA file: ", e$message
      )
    }
  )
}
