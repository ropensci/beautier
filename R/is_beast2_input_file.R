#' Is a file a valid BEAST2 input file?
#' @param filename name of the BEAST2 XML input file
#' @param verbose if TRUE, BEAST2 output is shown,
#'   no output otherwise
#' @return TRUE if the file is valid, FALSE if not
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{are_beast2_input_lines}} to check the lines
#' @export
is_beast2_input_file <- function(filename, verbose = FALSE) {
  if (!file.exists(filename)) {
    stop("file not found")
  }
  # Let BEAST2 run the created XML file
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar -validate",
    filename
  )

  # BEAST2 returns an error code.
  # An error code of 0 denotes that the file was valid
  status_code <- system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)

  # Invalid files are not valid BEAST2 input files
  if (status_code != 0) {
    if (verbose) {
      print(paste("status code:", status_code))
    }
    return(FALSE)
  }

  # Valid BEAST2 input files will result in an output with 'Done!' at the
  # last line
  output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
  if (verbose) {
    print(output)
  }
  is_valid <- utils::tail(output, n = 1) == "Done!"
  is_valid
}

#' Deprecated name for function \code{\link{is_beast2_input_file}}
#' @param filename name of the BEAST2 XML input file
#' @return TRUE if the file is valid, FALSE if not
#' @export
is_valid_beast2_input_file <- function(filename) {
  beautier::is_beast2_input_file(filename = filename)
}
