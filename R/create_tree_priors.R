#' Creates all supported tree priors,
#'   which is a list of the types returned by
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}},
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#' @return a list of tree_priors
#' @examples
#' check_empty_beautier_folder()
#'
#' tree_priors <- create_tree_priors()
#' # TRUE
#' is_bd_tree_prior(tree_priors[[1]])
#' is_cbs_tree_prior(tree_priors[[2]])
#' is_ccp_tree_prior(tree_priors[[3]])
#' is_cep_tree_prior(tree_priors[[4]])
#' is_yule_tree_prior(tree_priors[[5]])
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_tree_priors <- function() {
  list(
    beautier::create_bd_tree_prior(),
    beautier::create_cbs_tree_prior(),
    beautier::create_ccp_tree_prior(),
    beautier::create_cep_tree_prior(),
    beautier::create_yule_tree_prior()
  )
}
