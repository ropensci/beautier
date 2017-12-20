#' Determine if an object is a valid tree prior
#' @param x an object
#' @return TRUE if x is a valid tree_prior, FALSE otherwise
#' @seealso tree priors can be created by \code{\link{create_tree_prior}})
#' @author Richel J.C. Bilderbeek
is_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!is_tree_prior_name(x$name)) return(FALSE)
  if (!"id" %in% names(x)) return(FALSE)
  TRUE
}
