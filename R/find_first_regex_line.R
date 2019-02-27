#' Find the first line that satisfies a regex
#' @param lines lines of tex
#' @param regex the regex as text
#' @return index of the line
#' @author Richèl J.C. Bilderbeek
#' @noRd
find_first_regex_line <- function(lines, regex) {
  for (i in seq_along(lines)) {
    match <- stringr::str_extract(
      str = lines[i],
      pattern = regex
    )
    if (!is_one_na(match)) return(i) # nolint beautier function
  }
  NA
}
