#' Create a random phylogeny
#' @param n_taxa The number of taxa
#' @param taxa_name_ext the extension of the taxa names
#' @return a phylogeny of class \code{phylo}
#' @author Richel J.C. Bilderbeek
create_random_phylogeny <- function(
  n_taxa = n_taxa,
  taxa_name_ext = ""
) {
  phylogeny <- ape::rcoal(n = n_taxa)
  phylogeny$tip.label <- paste0(phylogeny$tip.label, taxa_name_ext) # nolint APE uses a different style guideline
  phylogeny
}
