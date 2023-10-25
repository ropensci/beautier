#' Get the prefix of operator IDs
#' @inheritParams default_params_doc
#' @return the prefix of operator IDs, similar to the name of a tree prior
#' @examples
#' check_empty_beautier_folder()
#'
#' # BirthDeath
#' get_operator_id_pre(
#'   tree_prior = create_bd_tree_prior()
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_operator_id_pre <- function(tree_prior) {
  if (is_bd_tree_prior(tree_prior)) {
    return("BirthDeath")
  }
  if (is_cbs_tree_prior(tree_prior)) {
    return("BayesianSkyline")
  }
  if (is_ccp_tree_prior(tree_prior)) {
    return("CoalescentConstant")
  }
  if (is_cep_tree_prior(tree_prior)) {
    return("CoalescentExponential")
  }
  if (is_yule_tree_prior(tree_prior)) {
    return("YuleModel")
  }
  stop("Unknown tree prior")
}
