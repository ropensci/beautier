find_last_regex_line <- function(lines, regex) {

  for (i in rev(seq_along(lines))) {
    match <- stringr::str_extract(
      str = lines[i],
      pattern = regex
    )
    if (!is.na(match)) return(i)
  }
  NA
}
