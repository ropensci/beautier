#' Determines if x is one NA
#' @param x the object to be determined if it is one NA
#' @return TRUE if x is one NA, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_na <- function(x) {
  if (is.function(x)) return(FALSE)
  length(x) == 1 && is.na(x)
}
