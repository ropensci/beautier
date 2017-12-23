#' Is a file a valid BEAST2 input file?
#' @inheritParams default_params_doc
#' @param filename name of the BEAST2 XML input file
#' @param beast_jar_path the path of \code{beast.jar}
#' @return TRUE if the file is valid, FALSE if not
#' @author Richel J.C. Bilderbeek
#' @seealso Use \code{\link{are_beast2_input_lines}} to check the lines
is_beast2_input_file <- function(
  filename,
  verbose = FALSE,
  beast_jar_path = "~/Programs/beast/lib/beast.jar"
) {
  if (!file.exists(filename)) {
    stop("file not found")
  }
  if (!file.exists(beast_jar_path)) {
    stop("BEAST2 not found at path '", beast_jar_path ,"'")
  }
  # Let BEAST2 run the created XML file
  cmd <- paste("java -jar", beast_jar_path, "-validate", filename)

  # BEAST2 returns an error code.
  # An error code of 0 denotes that the file was valid
  status_code <- system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)

  # Invalid files are not valid BEAST2 input files
  if (status_code != 0) {
    if (verbose) {
      print(paste("Non-zero status code:", status_code))
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
