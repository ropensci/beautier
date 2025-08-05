#' Find the first line that satisfies a regex
#' @param lines lines of text
#' @param regex the regex as text
#' @return index of the line
#' @author Richèl J.C. Bilderbeek
#' @export
find_first_regex_line <- function(lines, regex) {
  for (i in seq_along(lines)) {
    match <- stringr::str_extract(
      string = lines[i],
      pattern = regex
    )
    if (!beautier::is_one_na(match)) return(i)
  }
  NA
}
