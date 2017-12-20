#' Determine if the object is a valid
#'   constant coalescence population tree prior,
#'   as returned by \code{\link{create_ccp_tree_prior}}
#' @param x an object, to be determined if it is a valid
#'   constant coalescence population tree prior
#' @return TRUE if x is a valid constant coalescence population tree prior,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_ccp_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "coalescent_constant_population") return(FALSE)
  if (!"pop_size_distr" %in% names(x)) return(FALSE)
  TRUE
}
