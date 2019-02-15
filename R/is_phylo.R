#' Checks if the input is a phylogeny
#' @param x input to be checked
#' @return TRUE or FALSE
#' @seealso Use \link{check_phylogeny} to check for a phylogeny
#' @examples
#'   phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
#'   testit::assert(is_phylo(phylogeny))
#'
#'   testit::assert(!is_phylo("nonsense"))
#'   testit::assert(!is_phylo(NA))
#'   testit::assert(!is_phylo(NULL))
#' @author Richèl J.C. Bilderbeek
#' @export
is_phylo <- function(x) {
  class(x) == "phylo"
}
