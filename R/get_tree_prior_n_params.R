#' Get the number of parameters a tree prior has
#' @param tree_prior a tree_prior,
#'   as created by \code{\link{create_tree_prior}}
#' @return the number of parameters a tree prior has
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # birth_rate_distr is uniform, which has zero parameters
#'  # death_rate_distr is uniform, which has zero parameters
#'  testit::assert(
#'    beautier:::get_tree_prior_n_params(create_bd_tree_prior()) == 0
#'  )
#'
#'  # no distributions, no parameters
#'  testit::assert(
#'    beautier:::get_tree_prior_n_params(create_cbs_tree_prior()) == 0
#'  )
#'
#'  # pop_size_distr is 1/x, which has zero parameters
#'  testit::assert(
#'    beautier:::get_tree_prior_n_params(create_ccp_tree_prior()) == 0
#'  )
#'
#'  # pop_size_distr is 1/x, which has zero parameters
#'  # growth_rate_distr is Laplace, which has two parameters
#'  testit::assert(
#'    beautier:::get_tree_prior_n_params(create_cep_tree_prior()) == 2
#'  )
#'
#'  # birth_rate_distr is uniform, which has zero parameters
#'  testit::assert(
#'    beautier:::get_tree_prior_n_params(create_yule_tree_prior()) == 0
#'  )
#' @noRd
get_tree_prior_n_params <- function(
  tree_prior
) {
  if (!is_tree_prior(tree_prior)) { # nolint beautier function
    stop("'tree_prior' must be a tree prior")
  }
  if (is_bd_tree_prior(tree_prior)) { # nolint beautier function
    return(
      get_distr_n_params(tree_prior$birth_rate_distr) + # nolint beautier function
      get_distr_n_params(tree_prior$death_rate_distr) # nolint beautier function
    )
  } else if (is_cbs_tree_prior(tree_prior)) { # nolint beautier function
    return(0)
  } else if (is_ccp_tree_prior(tree_prior)) { # nolint beautier function
    return(get_distr_n_params(tree_prior$pop_size_distr)) # nolint beautier function
  } else if (is_cep_tree_prior(tree_prior)) { # nolint beautier function
    return(
      get_distr_n_params(tree_prior$pop_size_distr) + # nolint beautier function
      get_distr_n_params(tree_prior$growth_rate_distr) # nolint beautier function
    )
  } else {
    testit::assert(is_yule_tree_prior(tree_prior)) # nolint beautier function
    return(
      get_distr_n_params(tree_prior$birth_rate_distr) # nolint beautier function
    )
  }
}
