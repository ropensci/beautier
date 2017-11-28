#' Creates n Yule tree priors
#' @param n the number of Yule tree_prior objects
#' @return a list of Yule tree_prior objects
#' @examples
#'   m <- create_yule_tree_priors(ids = "some_id")
#'   testthat::expect_equal(length(m), 1)
#'   testthat::expect_true(is_yule_tree_prior(m[[1]]))
#'
#'   m <- create_yule_tree_priors(ids = c("a", "b"))
#'   testthat::expect_equal(length(m), 2)
#'   testthat::expect_true(is_yule_tree_prior(m[[1]]))
#'   testthat::expect_true(is_yule_tree_prior(m[[2]]))
#' @export
create_yule_tree_priors <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_yule_tree_prior(id = ids[i])
  }
  ms
}
