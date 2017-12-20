#' Checks if the input is a phylogeny
#' @param x input to be checked
#' @return TRUE or FALSE
#' @author Richel J.C. Bilderbeek
is_phylo <- function(x) {
  class(x) == "phylo"
}
