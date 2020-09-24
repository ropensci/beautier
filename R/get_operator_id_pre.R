#' Get the prefix of operator IDs
#' @inheritParams default_params_doc
#' @return the prefix of operator IDs, similar to the name of a tree prior
#' @examples
#' # BirthDeath
#' get_operator_id_pre(
#'   tree_prior = create_bd_tree_prior()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
get_operator_id_pre <- function(tree_prior) {
  if (beautier::is_bd_tree_prior(tree_prior)) {
    return("BirthDeath")
  }
  if (beautier::is_cbs_tree_prior(tree_prior)) {
    return("BayesianSkyline")
  }
  if (beautier::is_ccp_tree_prior(tree_prior)) {
    return("CoalescentConstant")
  }
  if (beautier::is_cep_tree_prior(tree_prior)) {
    return("CoalescentExponential")
  }
  if (beautier::is_yule_tree_prior(tree_prior)) {
    return("YuleModel")
  }
  stop("Unknown tree prior")
}
