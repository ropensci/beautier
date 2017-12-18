#' @author Richel J.C. Bilderbeek
#' @note TODO: remove
remove_first_brm_clock_rate <- function(lines) {
  # Find first line with '<branchRateModel id=\"RelaxedClock.c:'
  line_index <- NA
  for (i in seq_along(lines)) {
    match <- stringr::str_extract(
      str = lines[i],
      pattern = "<branchRateModel id=\"RelaxedClock.c:"
    )
    if (!is.na(match)) {
      line_index <- i
      break
    }
  }
  testit::assert(!is.na(line_index))

  # Remove 'clock.rate=\"@ucldMean.c:anthus_aco\" '
  from <- " clock\\.rate=\"@ucldMean\\.c:[a-zA-Z0-9_]+\""
  to <- ""
  str <- lines[line_index]
  lines[line_index] <- gsub(from, to, str)
  lines
}
