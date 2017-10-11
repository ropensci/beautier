#' Determine if an object is a valid tree_prior
#' @param x an object
#' @return TRUE if x is a valid tree_prior, FALSE otherwise
#' @export
is_valid_tree_prior <- function(
  x
) {
  if (!is_tree_prior_name(x$name))
  {
    return(FALSE)
  }
  return(TRUE)
}
