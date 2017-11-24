#' Get the number of distributions a tree prior has
#' @param tree_prior a tree_prior,
#'   as created by \code{\link{create_tree_prior}}
#' @return the number of distributions a tree prior has
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # birth_rate_distr and death_rate_distr
#'  testit::assert(get_tree_prior_n_distrs(create_bd_tree_prior()) == 2)
#'
#'  # none
#'  testit::assert(get_tree_prior_n_distrs(create_cbs_tree_prior()) == 0)
#'
#'  # pop_size_distr
#'  testit::assert(get_tree_prior_n_distrs(create_ccp_tree_prior()) == 1)
#'
#'  # pop_size_distr and growth_rate_distr
#'  testit::assert(get_tree_prior_n_distrs(create_cep_tree_prior()) == 2)
#'
#'  # birth_rate_distr
#'  testit::assert(get_tree_prior_n_distrs(create_yule_tree_prior()) == 1)
#' @export
get_tree_prior_n_distrs <- function(
  tree_prior
) {
  if (!is_tree_prior(tree_prior)) {
    stop("'tree_prior' must be a tree prior")
  }
  if (is_bd_tree_prior(tree_prior)) {
    return(2)
  } else if (is_cbs_tree_prior(tree_prior)) {
    return(0)
  } else if (is_ccp_tree_prior(tree_prior)) {
    return(1)
  } else if (is_cep_tree_prior(tree_prior)) {
    return(2)
  } else {
    testit::assert(is_yule_tree_prior(tree_prior)) # nolint internal function
    return(1)
  }
}