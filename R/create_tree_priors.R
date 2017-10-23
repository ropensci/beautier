#' Creates all supported tree priors
#' @return a list of tree_priors
#' @export
create_tree_priors <- function() {
  return(
    list(
      create_tree_prior(name = "yule"),
      create_tree_prior(name = "birth_death"),
      create_tree_prior(name = "coalescent_constant_population"),
      create_tree_prior(name = "coalescent_bayesian_skyline")
    )
  )
}
