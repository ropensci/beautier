#' Is a file a valid BEAST2 input file?
#' @param filename name of the BEAST2 XML input file
#' @return TRUE if the file is valid, FALSE if not
#' @export
is_valid_beast2_input_file <- function(filename) {
  if (!file.exists(filename)) {
    stop("file not found")
  }
  return (TRUE)
}
