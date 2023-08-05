#' Determines if the argument is a string
#' @param x the object to be determined of if it is one string
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_one_string("This is one string")
#'
#' # FALSE
#' is_one_string(NULL)
#' is_one_string(NA)
#' is_one_string(Inf)
#' is_one_string(314)
#' is_one_string(0)
#' is_one_string(-314)
#' is_one_string(3.14)
#' is_one_string(c("a", "b"))
#' is_one_string(is_one_string)
#' is_one_string(c())
#' is_one_string(c(1, 2))
#'
#' check_empty_beautier_folder()
#' @export
is_one_string <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (is.function(x)) return(FALSE)
  if (is.na(x)) return(FALSE)
  if (is.infinite(x)) return(FALSE)
  is.character(x)
}
