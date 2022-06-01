#' Determines if x is one NA
#' @param x the object to be determined if it is one NA
#' @return TRUE if x is one NA, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#'   testit::assert(is_one_na(NA))
#'
#' # FALSE
#'   testit::assert(!is_one_na(NULL))
#'   testit::assert(!is_one_na(42))
#'   testit::assert(!is_one_na("Hello"))
#'   testit::assert(!is_one_na(3.14))
#'   testit::assert(!is_one_na(c(NA, NA)))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_na <- function(x) {
  if (is.function(x)) return(FALSE)
  length(x) == 1 && is.na(x)
}
