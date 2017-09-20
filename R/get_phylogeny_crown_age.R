#' Obtain the crown age of a phylony
#' @param phylogeny The phylogeny to obtain the crown age of
#' @return the age of the phylogeny
#' @examples
#'   age <- 15
#'   set.seed(42)
#'   phylogeny <- PBD::pbd_sim(
#'     c(0.2, 1, 0.2, 0.0, 0.0), age
#'   )$tree
#'   n_taxa <- length(phylogeny$tip.label)
#'   testit::assert(n_taxa > 0)
#'   crown_age <- get_phylogeny_crown_age(phylogeny)
#'   testit::assert(all.equal(age, crown_age, tolerance = 0.001))
#' @author Richel Bilderbeek
#' @export
get_phylogeny_crown_age <- function(
  phylogeny
) {
  if (class(phylogeny) != "phylo") {
    stop(
      "get_phylogeny_crown_age: ",
      "phylogeny must be of class 'phylo'"
    )
  }
  n_taxa <- length(phylogeny$tip.label)
  testit::assert(n_taxa > 0)
  crown_age <- ape::dist.nodes(phylogeny)[n_taxa + 1][1]
  return(crown_age)
}
