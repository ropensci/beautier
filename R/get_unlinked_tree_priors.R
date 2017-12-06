#' Get the first tree prior of each ID
#' @param tree_priors a list of one or more tree priors
get_unlinked_tree_priors <- function(tree_priors) {
  testit::assert(beautier::are_tree_priors(tree_priors))
  results <- list()
  ids <- NULL
  for (tree_prior in tree_priors) {
    id <- tree_prior$id
    if (!id %in% ids) {
      ids <- c(ids, id)
      results[[length(ids)]] <- tree_prior
    }
  }

  testit::assert(beautier::are_tree_priors(results))
  results
}
