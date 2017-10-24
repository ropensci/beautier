#' Determine if the object is a valid constant coalescent Bayesian skyline prior
#' @param x an object, to be determined if it is a valid constant coalescent
#'   Bayesian skyline prior
#' @return TRUE if x is a valid constant coalescent Bayesian skyline prior,
#'   FALSE otherwise
#' @author Richel Bilderbeek
#' @export
is_cbs_tree_prior <- function(
  x
) {
  return("name" %in% names(x) && x$name == "coalescent_bayesian_skyline")
}
