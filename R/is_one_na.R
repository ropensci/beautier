#' Determines if x is one NA
#' @param x the object to be determined if it is one NA
#' @return TRUE if x is one NA, FALSE otherwise
#' @examples
#'   testit::assert(is_one_na(NA))
#'   testit::assert(!is_one_na(NULL))
#'   testit::assert(!is_one_na(42))
#'   testit::assert(!is_one_na("Hello"))
#'   testit::assert(!is_one_na(3.14))
#'   testit::assert(!is_one_na(c(NA, NA)))
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_na <- function(x) {
  if (is.function(x)) return(FALSE)
  length(x) == 1 && is.na(x)
}
