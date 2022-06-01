#' Checks if the input is a phylogeny
#' @param x input to be checked
#' @return TRUE or FALSE
#' @seealso Use \link{check_phylogeny} to check for a phylogeny
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
#' is_phylo(phylogeny)
#'
#' # FALSE
#' is_phylo("nonsense")
#' is_phylo(NA)
#' is_phylo(NULL)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_phylo <- function(x) {
  inherits(x, "phylo")
}
