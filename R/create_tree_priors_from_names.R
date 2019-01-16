#' Create tree priors from their names
#' @inheritParams default_params_doc
#' @examples
#'   names <- get_tree_prior_names()
#'   tree_priors <- create_tree_priors_from_names(names)
#'
#'   for (i in seq_along(names)) {
#'     testthat::expect_equal(names[i], tree_priors[[i]]$name)
#'   }
#' @author Richel J.C. Bilderbeek
#' @export
create_tree_priors_from_names <- function(tree_prior_names) {
  tree_priors <- list()
  for (i in seq_along(tree_prior_names)) {
    tree_prior_name <- tree_prior_names[i]
    tree_priors[[i]] <- create_tree_prior_from_name(tree_prior_name) # nolint beautier function
  }
  tree_priors
}
