#' Get the prefix of operator IDs
#' @param tree_prior tree priors, as created by \code{\link{create_tree_prior}}
#' @examples
#'   bd_pre <- get_operator_id_pre(
#'     tree_prior = create_bd_tree_prior()
#'   )
#'   testthat::expect_equal(bd_pre, "BirthDeath")
#' @author Richel J.C. Bilderbeek
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
