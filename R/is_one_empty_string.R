#' Determine if an object is one empty string
#' @param x the object that may be one string that may be empty
#' @return TRUE is `x` is one string that is empty
#' @examples
#' # TRUE
#' is_one_empty_string("")
#'
#' # FALSE
#' is_one_empty_string("3.14")
#' is_one_empty_string(c("", ""))
#' is_one_empty_string(42)
#' is_one_empty_string("nonsense")
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_empty_string <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (!is.character(x)) return(FALSE)
  nchar(x) == 0
}
