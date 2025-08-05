#' Check if the object is a list of one or more tree priors.
#'
#' Will \link{stop} if the object is not a list of one or more tree priors.
#' @param tree_priors the object to be checked if it is a list of one
#'   or more valid tree priors
#' @return nothing.
#'   Will \link{stop} if the object is not a list of one or more tree priors.
#' @seealso Use \link{create_tree_prior} to create a valid tree prior
#' @examples
#' check_empty_beautier_folder()
#'
#' check_tree_priors(create_yule_tree_prior())
#' check_tree_priors(list(create_yule_tree_prior()))
#' check_tree_priors(list(create_yule_tree_prior(), create_bd_tree_prior()))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_tree_priors <- function(tree_priors) {

  if (beautier::is_tree_prior(tree_priors)) {
    tree_priors <- list(tree_priors)
  }
  if (!beautier::are_tree_priors(tree_priors)) {
    stop(
      "'tree_priors' must be a list of one or more valid tree priors. ",
      "Actual value(s): ", tree_priors
    )
  }

}
