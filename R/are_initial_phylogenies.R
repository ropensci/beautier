#' Determine if x is exactly one phylogeny
#' @param phylos the object to check if it is exactly one phylogeny
#' @return TRUE if x is exactly one phylogeny
#' @examples
#'   phylogeny <- ape::read.tree(text = "(A:1,B:1);")
#'   testthat::expect_false(are_initial_phylogenies(phylogeny))
#'   testthat::expect_true(are_initial_phylogenies(NA))
#'   testthat::expect_true(are_initial_phylogenies(c(phylogeny)))
#'   testthat::expect_true(are_initial_phylogenies(c(phylogeny, phylogeny)))
#' @author Richèl J.C. Bilderbeek
#' @export
are_initial_phylogenies <- function(
  phylos
) {
  for (x in phylos) {
    if (!beautier::is_one_na(x) && !beautier::is_phylo(x)) return(FALSE)
  }
  TRUE
}
