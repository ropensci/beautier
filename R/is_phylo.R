#' Checks if the input is a phylogeny
#' @param x input to be checked
#' @return TRUE or FALSE
#' @author Richel Bilderbeek
#' @export
is_phylo <- function(x) {
  return(class(x) == "phylo")
}
