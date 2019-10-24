#' Check if the tree prior is a valid tree prior
#'
#' Calls \code{stop} if the tree priors are invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_tree_prior} to create a valid tree prior
#' @examples
#'  testthat::expect_silent(check_tree_prior(create_yule_tree_prior()))
#'  testthat::expect_silent(check_tree_prior(create_bd_tree_prior()))
#'  testthat::expect_silent(check_tree_prior(create_cbs_tree_prior()))
#'  testthat::expect_silent(check_tree_prior(create_ccp_tree_prior()))
#'  testthat::expect_silent(check_tree_prior(create_cep_tree_prior()))
#'
#'  # Can use list of one tree prior
#'  testthat::expect_silent(check_tree_prior(list(create_yule_tree_prior())))
#'
#'  # List of two tree priors is not a/one tree prior
#'  testthat::expect_error(
#'    check_tree_prior(
#'      list(create_yule_tree_prior(), create_yule_tree_prior())
#'    )
#'  )
#'
#'  # Must stop on non-tree priors
#'  testthat::expect_error(check_tree_prior(tree_prior = "nonsense"))
#'  testthat::expect_error(check_tree_prior(tree_prior = NULL))
#'  testthat::expect_error(check_tree_prior(tree_prior = NA))
#' @author Richèl J.C. Bilderbeek
#' @export
check_tree_prior <- function(tree_prior) {

  if (beautier::is_tree_prior(tree_prior)) {
    return()
  }
  if (length(tree_prior) == 1 && is_tree_prior(tree_prior[[1]])) { # nolint beautier function
    return()
  }
  stop(
    "'tree_prior' must be a valid tree prior.\n",
    "Actual value: ", tree_prior
  )
}
