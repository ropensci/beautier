#' Determine if MRCA priors that are monophyletic (i.e., assume
#' no other taxons share a same MRCA) are compatible, that
#' is, there are no taxa that intersect. For example, one
#' MRCA prior with taxon 1 and 2 is incompatible with an MRCA prior
#' with taxon 3 and 4
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
are_mrca_taxa_non_intersecting <- function(mrca_priors) {
  testit::assert(are_mrca_priors(mrca_priors))
  TRUE
}
