#' Determine if the object is a valid Yule tree prior
#' @param x an object, to be determined if it is a valid Yule tree prior
#' @return TRUE if x is a valid Yule tree prior, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_yule_tree_prior <- function(
  x
) {
  if (is.list(x) && length(x) == 1) return(is_yule_tree_prior(x[[1]]))
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "yule") return(FALSE)
  testit::assert("birth_rate_distr" %in% names(x))
  TRUE
}
