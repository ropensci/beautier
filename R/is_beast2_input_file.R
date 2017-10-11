#' Is a file a valid BEAST2 input file?
#' @param filename name of the BEAST2 XML input file
#' @return TRUE if the file is valid, FALSE if not
#' @export
is_beast2_input_file <- function(filename) {
  if (!file.exists(filename)) {
    stop("file not found")
  }
  # Let BEAST2 run the created XML file
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar -validate",
    filename
  )
  output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
  is_valid <- utils::tail(output, n = 1) == "Done!"
  is_valid
}

#' Deprecated name for function 'is_beast2_input_file'
#' @param filename name of the BEAST2 XML input file
#' @return TRUE if the file is valid, FALSE if not
#' @export
is_valid_beast2_input_file <- function(filename) {
  beastscriptr::is_beast2_input_file(filename = filename)
}
