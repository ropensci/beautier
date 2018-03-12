#' Determines if x is one NA
#' @param x the object to be determined if it is one NA
#' @return TRUE if x is one NA, FALSE otherwise
#' @examples
#'   testit::assert(beautier:::is_one_na(NA))
#'   testit::assert(!beautier:::is_one_na(NULL))
#'   testit::assert(!beautier:::is_one_na(42))
#'   testit::assert(!beautier:::is_one_na("Hello"))
#'   testit::assert(!beautier:::is_one_na(3.14))
#'   testit::assert(!beautier:::is_one_na(c(NA, NA)))
#' @author Richel J.C. Bilderbeek
is_one_na <- function(x) {
  length(x) == 1 && is.na(x)
}
