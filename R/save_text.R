#' Save text (a container of strings) to a file
#' @param filename filename of the file to have the text written to
#' @param text text to be written to file
#' @return Nothing
#' @export
#' @author Richel Bilderbeek
save_text <- function(filename, text) {
  my_file <- file(filename)
  writeLines(text, my_file)
  close(my_file)
}
