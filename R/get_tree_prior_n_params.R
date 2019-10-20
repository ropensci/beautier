#' Get the number of parameters a tree prior has
#' @param tree_prior a tree_prior,
#'   as created by \code{\link{create_tree_prior}}
#' @return the number of parameters a tree prior has
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # birth_rate_distr is uniform, which has zero parameters
#'  # death_rate_distr is uniform, which has zero parameters
#'  testit::assert(
#'    get_tree_prior_n_params(create_bd_tree_prior()) == 0
#'  )
#'
#'  # no distributions, no parameters
#'  testit::assert(
#'    get_tree_prior_n_params(create_cbs_tree_prior()) == 0
#'  )
#'
#'  # pop_size_distr is 1/x, which has zero parameters
#'  testit::assert(
#'    get_tree_prior_n_params(create_ccp_tree_prior()) == 0
#'  )
#'
#'  # pop_size_distr is 1/x, which has zero parameters
#'  # growth_rate_distr is Laplace, which has two parameters
#'  testit::assert(
#'    get_tree_prior_n_params(create_cep_tree_prior()) == 2
#'  )
#'
#'  # birth_rate_distr is uniform, which has zero parameters
#'  testit::assert(
#'    get_tree_prior_n_params(create_yule_tree_prior()) == 0
#'  )
#' @export
get_tree_prior_n_params <- function(
  tree_prior
) {
  if (!beautier::is_tree_prior(tree_prior)) {
    stop("'tree_prior' must be a tree prior")
  }
  if (beautier::is_bd_tree_prior(tree_prior)) {
    return(
      beautier::get_distr_n_params(tree_prior$birth_rate_distr) +
      beautier::get_distr_n_params(tree_prior$death_rate_distr)
    )
  } else if (beautier::is_cbs_tree_prior(tree_prior)) {
    return(0)
  } else if (beautier::is_ccp_tree_prior(tree_prior)) {
    return(beautier::get_distr_n_params(tree_prior$pop_size_distr))
  } else if (beautier::is_cep_tree_prior(tree_prior)) {
    return(
      beautier::get_distr_n_params(tree_prior$pop_size_distr) +
      beautier::get_distr_n_params(tree_prior$growth_rate_distr)
    )
  } else {
    testit::assert(beautier::is_yule_tree_prior(tree_prior))
    return(
      beautier::get_distr_n_params(tree_prior$birth_rate_distr)
    )
  }
}
