#' Determine if MRCA priors that are monophyletic (i.e., assume
#' no other taxons share a same MRCA) are compatible, that
#' is, there are no taxa that intersect.
#'
#' For example, one
#' MRCA prior with taxon 1 and 2 is incompatible with an MRCA prior
#' with taxon 3 and 4
#' @inheritParams default_params_doc
#' @return TRUE if the MRCA priors are non-intersecting.
#'   FALSE otherwise.
#' @author Richèl J.C. Bilderbeek
#' @noRd
are_mrca_taxa_non_intersecting <- function(mrca_priors) {
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  if (is_one_na(mrca_priors)) return(TRUE) # nolint beautier function
  # mrca_prior_1 must be monophyletic, mrca_prior_2 may be
  for (mrca_prior_1 in mrca_priors) {
    testit::assert(is_mrca_prior(mrca_prior_1)) # nolint beautier function
    testit::assert(!is_one_na(mrca_prior_1)) # nolint beautier function
    testit::assert(!is_one_na(mrca_prior_1$taxa_names)) # nolint beautier function
    taxa_names_1 <- mrca_prior_1$taxa_names
    for (mrca_prior_2 in mrca_priors) {
      testit::assert(is_mrca_prior(mrca_prior_2)) # nolint beautier function
      testit::assert(!is_one_na(mrca_prior_2)) # nolint beautier function
      testit::assert(!is_one_na(mrca_prior_2$taxa_names)) # nolint beautier function
      taxa_names_2 <- mrca_prior_2$taxa_names
      if (all(taxa_names_2 %in% taxa_names_1)) next
      if (all(taxa_names_1 %in% taxa_names_2)) next
      diff <- setdiff(x = taxa_names_1, y = taxa_names_2)
      testit::assert(length(diff) != 0)
      if (length(diff) != length(taxa_names_1) &&
          length(diff) != length(taxa_names_2)
      ) {
        return(FALSE)
      }
    }
  }
  TRUE
}
