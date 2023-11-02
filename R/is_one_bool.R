#' Check if the argument is one boolean
#' @param x the argument to be tested to be boolean
#' @return TRUE if the argument is one boolean, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_one_bool(TRUE)
#' is_one_bool(FALSE)
#'
#' # FALSE
#' is_one_bool(NULL)
#' is_one_bool(NA)
#' is_one_bool(c())
#' is_one_bool("nonsense")
#' is_one_bool(is_one_bool)
#' is_one_bool(c(TRUE, FALSE))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_bool <- function(x) {
  isTRUE(x) || isFALSE(x)
}
