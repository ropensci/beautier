#' Creates all supported tree priors,
#'   which is a list of the types returned by
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}},
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#' @return a list of tree_priors
#' @examples
#'   tree_priors <- create_tree_priors()
#'   testit::assert(beautier:::is_bd_tree_prior(tree_priors[[1]]))
#'   testit::assert(beautier:::is_cbs_tree_prior(tree_priors[[2]]))
#'   testit::assert(beautier:::is_ccp_tree_prior(tree_priors[[3]]))
#'   testit::assert(beautier:::is_cep_tree_prior(tree_priors[[4]]))
#'   testit::assert(beautier:::is_yule_tree_prior(tree_priors[[5]]))
#' @author Richèl J.C. Bilderbeek
#' @export
create_tree_priors <- function() {
  list(
    beautier::create_bd_tree_prior(), # nolint beautier function
    beautier::create_cbs_tree_prior(), # nolint beautier function
    beautier::create_ccp_tree_prior(), # nolint beautier function
    beautier::create_cep_tree_prior(), # nolint beautier function
    beautier::create_yule_tree_prior() # nolint beautier function
  )
}
