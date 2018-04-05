#' Determine if x is exactly one phylogeny
#' @param x the object to check if it is exactly one phylogeny
#' @return TRUE if x is exactly one phylogeny
#' @author Richel J.C. Bilderbeek
#' @noRd
are_initial_phylogenies <- function(
  phylos
) {
  for (x in phylos) {
    if (!is.na(x) && !is_phylo(x)) return(FALSE)
  }
  TRUE
}
