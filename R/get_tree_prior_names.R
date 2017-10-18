#' Get the tree prior names
#' @return the tree prior names
#' @export
get_tree_prior_names <- function() {
  return(
    c(
      "birth_death",
      "coalescent_bayesian_skyline",
      "coalescent_constant_population",
      "yule"
    )
  )
}
