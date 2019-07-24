#' Check if the phylogeny is a valid phylogeny object.
#'
#' Calls \code{stop} if the phylogeny is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link[ape]{read.tree} to create a phylogeny
#' @examples
#' library(testthat)
#'
#' # Must do nothing on phylogenies
#' phylogeny <- ape::read.tree(text = "(A:1, B:1):1;")
#' expect_silent(check_phylogeny(phylogeny))
#'
#' # Must stop on non-phylogenies
#' expect_error(check_phylogeny("nonsense"))
#' expect_error(check_phylogeny(NULL))
#' expect_error(check_phylogeny(NA))
#' expect_error(check_phylogeny(c()))
#' expect_error(check_phylogeny(c(3, 1, 4)))
#' @author Richèl J.C. Bilderbeek
#' @export
check_phylogeny <- function(phylogeny) {
  if (class(phylogeny) == "phylo") {
    return()
  }
  stop(
    "'phylogeny' must be a valid phylogeny.\n",
    "Actual value: ", phylogeny
  )
}
