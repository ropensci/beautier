#' Create a Most Recent Common Ancestor prior
#' @inheritParams default_params_doc
#' @param name the unique name of the MRCA prior, for example a genus, family,
#'   order or even class name. Leave at NA to have it named automatically
#' @param taxa_names names of the taxa,
#'   as returned by \code{\link{get_taxa_names}}.
#'   Keep at \code{NA} to have it initialized automatically
#' @param mrca_distr the distribution used by the MRCA prior.
#'   Can be NA (the default) or any distribution
#'   returned by \code{\link{create_distr}}
#' @return an MRCA prior
#' @examples
#'  fasta_filename <- get_beautier_path("anthus_aco.fas")
#'
#'  # The first two taxa are sister species
#'  mrca_prior <- create_mrca_prior(
#'    alignment_id = get_alignment_id(fasta_filename = fasta_filename),
#'    taxa_names = get_taxa_names(filename = fasta_filename)[1:2]
#'  )
#'
#'  # Set the crown age
#'  mrca_prior <- create_mrca_prior(
#'    alignment_id = get_alignment_id(fasta_filename = fasta_filename),
#'    taxa_names = get_taxa_names(filename = fasta_filename),
#'    is_monophyletic = TRUE
#'  )
#' @author Richel J.C. Bilderbeek
#' @export
create_mrca_prior <- function(
  alignment_id = NA,
  taxa_names = NA,
  is_monophyletic = FALSE,
  mrca_distr = NA,
  name = NA,
  clock_prior_distr_id = NA
) {
  if (length(name) != 1 || (!is.character(name) && !is.na(name))) {
    stop("'name' must be NA or characters")
  }
  if (!is_one_na(alignment_id) && !is.character(alignment_id)) {
    stop("'alignment_id' must be NA or characters")
  }
  if (!is_one_na(taxa_names) && !is.vector(taxa_names, mode = "character")) {
    stop("'taxa_names' must a character vector")
  }
  if (!is.logical(is_monophyletic)) {
    stop("'is_monophyletic' must be either TRUE or FALSE")
  }
  if (!is_distr(mrca_distr) && !is.na(mrca_distr)) { # nolint internal function
    stop("'mrca_distr' must a distribution, as created by 'create_distr'")
  }
  testit::assert(length(taxa_names) > 0)
  if (!is_one_na(taxa_names) && sum(taxa_names == "") > 0) {
    stop("'taxa_names' must be NA or have at least one taxon name")
  }
  if (!is_one_na(taxa_names) && length(unique(taxa_names)) != length(taxa_names)) {
    stop("'taxa_names' must be NA or all names must be unique")
  }

  mrca_prior <- list(
    name = name,
    alignment_id = alignment_id,
    taxa_names = taxa_names,
    is_monophyletic = is_monophyletic,
    mrca_distr = mrca_distr,
    clock_prior_distr_id = clock_prior_distr_id
  )
  testit::assert(is_mrca_prior(mrca_prior)) # nolint internal function
  mrca_prior
}
