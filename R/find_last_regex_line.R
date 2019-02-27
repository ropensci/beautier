#' Find the index of the last line that matches a regex
#' @param lines lines of text
#' @param regex regex string
#' @return index of the line
#' @author Richèl J.C. Bilderbeek
#' @noRd
find_last_regex_line <- function(lines, regex) {

  for (i in rev(seq_along(lines))) {
    match <- stringr::str_extract(
      str = lines[i],
      pattern = regex
    )
    if (!is_one_na(match)) return(i) # nolint beautier function
  }
  NA
}
