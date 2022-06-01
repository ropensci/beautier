#' Obtain the crown age of a phylogeny.
#'
#' The crown age of a phylogeny is the time between
#' the present and the moment of at which the first
#' diversification (resulting in two lineages) happened.
#' @param phylogeny The phylogeny to obtain the crown age of
#' @return the crown age of the phylogeny
#' @examples
#' check_empty_beautier_folder()
#'
#' phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
#' get_crown_age(phylogeny = phylogeny)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_crown_age <- function(
  phylogeny
) {
  if (!beautier::is_phylo(phylogeny) && !inherits(phylogeny, "multiPhylo")) {
    stop("phylogeny must be of class 'phylo' or multiPhylo")
  }
  if (!ape::is.ultrametric(phylogeny)) {
    stop("'phylogeny' must be ultrametric")
  }

  n_taxa <- length(phylogeny$tip.label)
  testit::assert(n_taxa > 0)
  ape::dist.nodes(phylogeny)[n_taxa + 1][1]
}
