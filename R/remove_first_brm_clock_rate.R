#' Remove the first \code{clock.rate} element
#' from a \code{branchRateModel} section
#' @author Richel J.C. Bilderbeek
#' @note TODO: remove
#' @examples
#'   input <- "<branchRateModel id=\"RelaxedClock.c:anthus_aco\" clock.rate=\"@ucldMean.c:anthus_aco\"/>" # nolint XML
#'   created <- beautier:::remove_first_brm_clock_rate(input)
#'   expected <- "<branchRateModel id=\"RelaxedClock.c:anthus_aco\"/>" # nolint XML
#'   testit::assert(created == expected)
#' @noRd
remove_first_brm_clock_rate <- function(lines) {
  # Find first line with '<branchRateModel id=\"RelaxedClock.c:'
  line_index <- NA
  for (i in seq_along(lines)) {
    match <- stringr::str_extract(
      string = lines[i],
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
