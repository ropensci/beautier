#' Check if the tree prior is a valid tree prior
#'
#' Calls \code{stop} if the tree priors are invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_tree_prior} to create a valid tree prior
#' @examples
#' check_empty_beautier_folder()
#'
#' check_tree_prior(create_yule_tree_prior())
#' check_tree_prior(create_bd_tree_prior())
#' check_tree_prior(create_cbs_tree_prior())
#' check_tree_prior(create_ccp_tree_prior())
#' check_tree_prior(create_cep_tree_prior())
#'
#' # Can use list of one tree prior
#' check_tree_prior(list(create_yule_tree_prior()))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_tree_prior <- function(tree_prior) {

  if (is_tree_prior(tree_prior)) {
    return()
  }
  if (length(tree_prior) == 1 && is_tree_prior(tree_prior[[1]])) {
    return()
  }
  stop(
    "'tree_prior' must be a valid tree prior.\n",
    "Actual value: ", tree_prior
  )
}
