#' Creates all supported tree priors
#' @return a list of tree_priors
#' @export
create_tree_priors <- function() {
  return(
    list(
      beastscriptr::create_bd_tree_prior(),
      beastscriptr::create_cbs_tree_prior(),
      beastscriptr::create_ccp_tree_prior(),
      beastscriptr::create_yule_tree_prior()
    )
  )
}
