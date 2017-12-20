#' Would these lines of text, when written to a file,
#'   result in a valid BEAST2 input file?
#' @param lines lines of text
#' @param method the method to check. Can be 'deep' or 'fast'.
#'   The 'deep' method uses BEAST2 to validate the complete file.
#'   The 'fast' method uses some superficial tests (for example: if all
#'   IDs are unique)
#' @param verbose if TRUE, BEAST2 output is shown,
#'   no output otherwise
#' @return TRUE if the text is valid, FALSE if not
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{is_beast2_input_file}} to check a file
are_beast2_input_lines <- function(
  lines,
  verbose = FALSE,
  method = ifelse(is_on_travis(), "deep", "fast")
) {
  if (!method %in% c("deep", "fast")) {
    stop("'method' must be \"deep\" or \"fast\", value was '", method, "'")
  }
  if (method == "deep") {
    filename <- tempfile()
    beautier::save_lines(filename = filename, lines = lines)
    return(
      are_beast2_input_lines_deep(lines = lines, verbose = verbose)
    )
  } else {
    testit::assert(method == "fast")
    return(
      are_beast2_input_lines_fast(lines) # nolint internal function
    )
  }
}



#' Would these lines of text, when written to a file,
#'   result in a valid BEAST2 input file?
#' @param lines lines of text
#' @param verbose if TRUE, BEAST2 output is shown,
#'   no output otherwise
#' @return TRUE if the text is valid, FALSE if not
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{is_beast2_input_file}} to check a file
#' @examples
#'   fasta_filename <- beautier::get_path("test_output_0.fas")
#'   lines <- create_beast2_input(fasta_filename)
are_beast2_input_lines_deep <- function(
  lines,
  verbose = FALSE
) {
  filename <- tempfile()
  beautier::save_lines(filename = filename, lines = lines)
  is_beast2_input_file(
    filename = filename,
    verbose = verbose
  )
}

#' Would these lines of text, when written to a file,
#'   result in a valid BEAST2 input file?
#' @param lines lines of text
#' @return TRUE if the text is valid, FALSE if not
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{is_beast2_input_file}} to check a file
are_beast2_input_lines_fast <- function(
  lines
) {
  has_unique_ids(lines) # nolint internal function
}
