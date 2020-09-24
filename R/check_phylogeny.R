#' Check if the phylogeny is a valid phylogeny object.
#'
#' Calls \code{stop} if the phylogeny is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \code{ape::read.tree} to create a phylogeny
#' @examples
#'
#' # Must do nothing on phylogenies
#' phylogeny <- ape::read.tree(text = "(A:1, B:1):1;")
#' check_phylogeny(phylogeny)
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
