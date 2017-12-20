#' Determine if the object is a valid Yule tree prior,
#'   as returned by \code{\link{create_yule_tree_prior}}
#' @param x an object, to be determined if it is a valid Yule tree prior
#' @return TRUE if x is a valid Yule tree prior, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_yule_tree_prior <- function(
  x
) {
  if (is.list(x) && length(x) == 1) return(is_yule_tree_prior(x[[1]]))
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "yule") return(FALSE)
  if (!"birth_rate_distr" %in% names(x)) return(FALSE)
  TRUE
}
