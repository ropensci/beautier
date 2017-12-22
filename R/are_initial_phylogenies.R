are_initial_phylogenies <- function(
  phylos
) {
  for (x in phylos) {
    if (!is.na(x) && !is_phylo(x)) return(FALSE)
  }
  TRUE
}
