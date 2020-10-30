#' Create a Most Recent Common Ancestor prior
#' @inheritParams default_params_doc
#' @param name the unique name of the MRCA prior, for example a genus, family,
#' order or even class name.
#' Leave at \link{NA} to have it named automatically.
#' @param mrca_distr the distribution used by the MRCA prior.
#'   Can be NA (the default) or any distribution
#'   returned by \code{\link{create_distr}}
#' @return an MRCA prior
#' @examples
#'  fasta_filename <- get_beautier_path("anthus_aco.fas")
#'
#'  # The first two taxa are sister species
#'  mrca_prior <- create_mrca_prior(
#'    taxa_names = get_taxa_names(filename = fasta_filename)[1:2]
#'  )
#'
#'  # The taxa are monophyletic
#'  mrca_prior <- create_mrca_prior(
#'    taxa_names = get_taxa_names(filename = fasta_filename),
#'    is_monophyletic = TRUE
#'  )
#'
#'  # Set the crown age to 10
#'  mrca_prior <- create_mrca_prior(
#'    taxa_names = get_taxa_names(fasta_filename),
#'    mrca_distr = create_normal_distr(mean = 10, sigma = 0.1)
#'  )
#' @author Richèl J.C. Bilderbeek
#' @export
create_mrca_prior <- function(
  alignment_id = NA,
  taxa_names = NA,
  is_monophyletic = FALSE,
  mrca_distr = NA,
  name = NA,
  clock_prior_distr_id = NA
) {
  mrca_prior <- list(
    name = name,
    alignment_id = alignment_id,
    taxa_names = taxa_names,
    is_monophyletic = is_monophyletic,
    mrca_distr = mrca_distr,
    clock_prior_distr_id = clock_prior_distr_id
  )
  beautier::check_mrca_prior(mrca_prior)
  mrca_prior
}
