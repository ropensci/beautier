#' Determine if the object is a valid constant coalescent Bayesian skyline prior
#'   as returned by \code{\link{create_cbs_tree_prior}}
#' @param x an object, to be determined if it is a valid constant coalescent
#'   Bayesian skyline prior
#' @return TRUE if x is a valid constant coalescent Bayesian skyline prior,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_cbs_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "coalescent_bayesian_skyline") return(FALSE)
  if (!"group_sizes_dimension" %in% names(x)) return(FALSE)
  TRUE
}
