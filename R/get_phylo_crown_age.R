#' Obtain the crown age of a phylony
#' @param phylogeny The phylogeny to obtain the crown age of
#' @return the age of the phylogeny
#' @examples
#'   phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
#'   created <- beautier:::get_phylo_crown_age(phylogeny = phylogeny)
#'   testit::assert(created == 15)
#' @author Richel J.C. Bilderbeek
get_phylo_crown_age <- function(
  phylogeny
) {
  if (class(phylogeny) != "phylo" && class(phylogeny) != "multiPhylo") {
    stop("phylogeny must be of class 'phylo' or multiPhylo")
  }
  n_taxa <- length(phylogeny$tip.label)
  testit::assert(n_taxa > 0)
  crown_age <- ape::dist.nodes(phylogeny)[n_taxa + 1][1]
  crown_age
}
