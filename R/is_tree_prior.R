#' Determine if an object is a valid tree_prior
#' @param x an object
#' @return TRUE if x is a valid tree_prior, FALSE otherwise
#' @export
is_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) {
    return(FALSE)
  }
  if (!is_tree_prior_name(x$name)) {
    return(FALSE)
  }
  return(TRUE)
}
