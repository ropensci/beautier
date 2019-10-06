#' Multi-file file.exists function
#' @param filenames one or more filename
#' @return TRUE if all files exists, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' filenames <- c(get_fasta_filename(), get_fasta_filename())
#' expect_true(files_exist(filenames))
#'
#' filenames <- c(get_fasta_filename(), tempfile())
#' expect_false(files_exist(filenames))
#' @export
files_exist <- function(filenames) {
  for (filename in filenames) {
    if (!file.exists(filename)) {
      return(FALSE)
    }
  }
  TRUE
}
