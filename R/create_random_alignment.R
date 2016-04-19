#' Create a random alignment
#' @param n_taxa The number of taxa
#' @param sequence_length The number of base pairs the alignment will have
#' @param rate mutation rate
#' @return an alignment of class DNAbin
#' @export
create_random_alignment <- function(
  n_taxa,
  sequence_length,
  rate = 1
) {
  if (n_taxa < 2) {
    stop("create_random_alignment: ",
         "need n_taxa >= 2, ",
         "instead of ", n_taxa
    )
  }
  if (n_taxa < 2) {
    stop("create_random_alignment: ",
         "need sequence_length >= 1, ",
         "instead of ", sequence_length
    )
  }
  if (rate < 0.0 || rate > 1.0) {
    stop("create_random_alignment: ",
         "rate needs to be [0.0, 1.0], ",
         "instead of ", rate
    )
  }
  phylogeny <- ape::rcoal(n = n_taxa)
  alignments_phydat <- phangorn::simSeq(
    phylogeny, l = sequence_length, rate = rate
  )
  alignments_dnabin <- ape::as.DNAbin(alignments_phydat)
  alignments_dnabin
}
