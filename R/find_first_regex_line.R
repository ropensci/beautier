#' Find the first line that satisfies a regex
#' @param lines lines of tex
#' @param regex the regex as text
#' @author Richel J.C. Bilderbeek
find_first_regex_line <- function(lines, regex) {
  for (i in seq_along(lines)) {
    match <- stringr::str_extract(
      str = lines[i],
      pattern = regex
    )
    if (!is.na(match)) return(i)
  }
  NA
}
