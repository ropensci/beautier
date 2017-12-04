#' Save text (a container of strings) to a file
#' @param filename filename of the file to have the text written to
#' @param lines lines of text to be written to file
#' @return Nothing
#' @author Richel J.C. Bilderbeek
#' @export
save_lines <- function(filename, lines) {
  my_file <- file(filename)
  writeLines(lines, my_file)
  close(my_file)
}
