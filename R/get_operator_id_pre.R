#' Get the prefix of operator IDs
#' @param tree_priors tree priors, as created by create_tree_prior
#' @examples
#'   bd_pre <- get_operator_id_pre(
#'     tree_priors = create_tree_prior(name = "birth_death")
#'   )
#'   testthat::expect_equal(bd_pre, "BirthDeath")
#'   ccp_pre <- get_operator_id_pre(
#'     tree_priors = create_tree_prior(name = "coalescent_constant_population")
#'   )
#'   testthat::expect_equal(ccp_pre, "CoalescentConstant")
#' @author Richel Bilderbeek
#' @export
get_operator_id_pre <- function(tree_priors) {
  if (is_yule_tree_prior(tree_priors)) {
    return("YuleModel")
  }
  if (is_bd_tree_prior(tree_priors)) {
    return("BirthDeath")
  }
  if (is_ccp_tree_prior(tree_priors)) {
    return("CoalescentConstant")
  }
  stop("Unknown tree prior")
}
