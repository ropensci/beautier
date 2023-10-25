#' Check if the phylogeny is a valid phylogeny object.
#'
#' Calls \code{stop} if the phylogeny is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \code{ape::read.tree} to create a phylogeny
#' @examples
#' check_empty_beautier_folder()
#'
#' # Must do nothing on phylogenies
#' phylogeny <- ape::read.tree(text = "(A:1, B:1):1;")
#' check_phylogeny(phylogeny)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_phylogeny <- function(phylogeny) {
  if (is_phylo(phylogeny)) {
    return()
  }
  stop(
    "'phylogeny' must be a valid phylogeny.\n",
    "Actual value: ", phylogeny
  )
}
