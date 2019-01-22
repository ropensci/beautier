#' Check if the phylogeny is a valid phylogeny object.
#'
#' Calls \code{stop} if the phylogeny is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @examples
#'  phylogeny <- ape::read.tree(text = "((A:1, B:1):1;")
#'  testthat::expect_silent(check_phylo(phylogeny))
#'
#'  # Must stop on non-phylogenies
#'  testthat::expect_error(check_phylo(phylo = "nonsense"))
#'  testthat::expect_error(check_phylo(phylo = NULL))
#'  testthat::expect_error(check_phylo(phylo = NA))
#' @author Richel J.C. Bilderbeek
#' @export
check_phylo <- function(phylo) {
  if (class(phylo) == "phylo") {
    return()
  }
  stop(
    "'phylo' must be a valid phylogeny.\n",
    "Actual value: ", phylo
  )
}
