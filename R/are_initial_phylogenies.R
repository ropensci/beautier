#' Determine if x is exactly one phylogeny
#' @param x the object to check if it is exactly one phylogeny
#' @return TRUE if x is exactly one phylogeny
#' @examples
#'   phylogeny <- ape::read.tree(text = "(A:1,B:1);")
#'   testthat::expect_false(are_initial_phylogenies(phylogeny))
#'   testthat::expect_true(are_initial_phylogenies(NA))
#'   testthat::expect_true(are_initial_phylogenies(c(phylogeny)))
#'   testthat::expect_true(are_initial_phylogenies(c(phylogeny, phylogeny)))
#' @author Richèl J.C. Bilderbeek
#' @noRd
are_initial_phylogenies <- function(
  phylos
) {
  for (x in phylos) {
    if (!is_one_na(x) && !is_phylo(x)) return(FALSE) # nolint beautier function
  }
  TRUE
}
