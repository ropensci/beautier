#' Would these lines of text, when written to a file,
#'   result in a valid BEAST2 input file?
#' @param text lines of text
#' @param verbose if TRUE, BEAST2 output is shown,
#'   no output otherwise
#' @return TRUE if the text is valid, FALSE if not
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{is_beast2_input_file}} to check a file
#' @export
are_beast2_input_lines <- function(text, verbose = FALSE) {

  filename <- tempfile()
  beautier::save_text(filename = filename, text = text)
  beautier::is_beast2_input_file(filename, verbose = verbose)
}
