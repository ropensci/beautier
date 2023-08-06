#' General function to create a distribution.
#' @param x the object that may be one string that may be a number
#' @return TRUE is `x` is one string that is a number
#' @examples
#' # TRUE
#' is_one_string_that_is_a_number("3.14")
#'
#' # FALSE
#' is_one_string_that_is_a_number(c("3.14", "42"))
#' is_one_string_that_is_a_number("")
#' is_one_string_that_is_a_number(42)
#' is_one_string_that_is_a_number("nonsense")
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_string_that_is_a_number <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (!is.character(x)) return(FALSE)
  y <- suppressWarnings(as.numeric(x))
  !is.na(y)
}
