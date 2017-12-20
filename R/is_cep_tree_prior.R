#' Determine if the object is a valid
#' coalescent exponential population tree prior
#'   as returned by \code{\link{create_cep_tree_prior}}
#' @param x an object, to be determined if it is a valid
#'   constant coalescent exponential population tree prior
#' @return TRUE if x is a valid coalescent exponentialpopulation tree prior,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_cep_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "coalescent_exp_population") return(FALSE)
  if (!"pop_size_distr" %in% names(x)) return(FALSE)
  if (!"growth_rate_distr" %in% names(x)) return(FALSE)
  TRUE
}
