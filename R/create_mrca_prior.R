#' Create a Most Recent Common Ancestor prior
#' @inheritParams default_params_doc
#' @param name the unique name of the MRCA prior, for example a genus, family,
#'   order or even class name. Leave at NA to have it named automatically
#' @param taxa_names names of the taxa,
#'   as returned by \code{\link{get_taxa_names}}
#' @param mrca_distr the distribution used by the MRCA prior.
#'   Can be NA (the default) or any distribution
#'   returned by \code{\link{create_distr}}
#' @return an MRCA prior
#' @author Richel J.C. Bilderbeek
#' @export
create_mrca_prior <- function(
  alignment_id,
  taxa_names,
  is_monophyletic = FALSE,
  mrca_distr = NA,
  name = NA,
  clock_prior_distr_id = NA
) {
  if (length(name) != 1 || (!is.character(name) && !is.na(name))) {
    stop("'name' must be NA or characters")
  }
  if (!is.character(alignment_id)) {
    stop("'alignment_id' must be characters")
  }
  if (!is.vector(taxa_names, mode = "character")) {
    stop("'taxa_names' must a character vector")
  }
  if (!is.logical(is_monophyletic)) {
    stop("'is_monophyletic' must be either TRUE or FALSE")
  }
  if (!is_distr(mrca_distr) && !is.na(mrca_distr)) {
    stop("'mrca_distr' must a distribution, as created by 'create_distr'")
  }
  testit::assert(length(taxa_names) > 0)
  if (sum(taxa_names == "") > 0) {
    stop("'taxa_names' must have at least one taxon name")
  }
  if (length(unique(taxa_names)) != length(taxa_names)) {
    stop("All names of 'taxa_names' must be unique")
  }

  mrca_prior <- list(
    name = name,
    alignment_id = alignment_id,
    taxa_names = taxa_names,
    is_monophyletic = is_monophyletic,
    mrca_distr = mrca_distr,
    clock_prior_distr_id = clock_prior_distr_id
  )
  testit::assert(is_mrca_prior(mrca_prior))
  mrca_prior
}
