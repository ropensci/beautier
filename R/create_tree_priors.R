#' Creates all supported tree priors
#' @return a list of tree_priors
#' @export
create_tree_priors <- function() {
  return(
    list(
      beautier::create_bd_tree_prior(),
      beautier::create_cbs_tree_prior(),
      beautier::create_ccp_tree_prior(),
      beautier::create_cep_tree_prior(),
      beautier::create_yule_tree_prior()
    )
  )
}
