#' Remove consecutive lines
#' @param text lines of characters
#' @param lines_to_remove lines of character that need to be removed from text
#' @return lines of text
#' @author Richèl J.C. Bilderbeek
#' @export
remove_multiline <- function(text, lines_to_remove) {
  first_line_to_remove <- lines_to_remove[1]
  first_line_to_remove_index <- which(text == first_line_to_remove)
  before_last_index <- first_line_to_remove_index - 1
  after_start_index <- first_line_to_remove_index + length(lines_to_remove)
  before <- NULL
  if (before_last_index >= 1) before <- text[1:before_last_index]
  after <- NULL
  if (after_start_index <= length(text)) {
    after <- text[after_start_index:length(text)]
  }
  text <- c(before, after)
  text
}
