#' Get the prefix of operator IDs
#' @inheritParams default_params_doc
#' @examples
#'   bd_pre <- beautier:::get_operator_id_pre(
#'     tree_prior = create_bd_tree_prior()
#'   )
#'   testthat::expect_equal(bd_pre, "BirthDeath")
#' @author Richel J.C. Bilderbeek
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
