#' Get the first tree prior of each ID
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
get_unlinked_tree_priors <- function(tree_priors) {
  testit::assert(are_tree_priors(tree_priors))
  results <- list()
  ids <- NULL
  for (tree_prior in tree_priors) {
    id <- tree_prior$id
    if (!id %in% ids) {
      ids <- c(ids, id)
      results[[length(ids)]] <- tree_prior
    }
  }

  testit::assert(are_tree_priors(results))
  results
}
