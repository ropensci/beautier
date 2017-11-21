#' Creates all supported tree priors,
#'   which is just a list of the types returned by
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}},
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#' @return a list of tree_priors
#' @author Richel J.C. Bilderbeek
#' @examples
#'   tree_priors <- create_tree_priors()
#'   testit::assert(is_bd_tree_prior(tree_priors[[1]]))
#'   testit::assert(is_cbs_tree_prior(tree_priors[[2]]))
#'   testit::assert(is_ccp_tree_prior(tree_priors[[3]]))
#'   testit::assert(is_cep_tree_prior(tree_priors[[4]]))
#'   testit::assert(is_yule_tree_prior(tree_priors[[5]]))
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
