#' Check if the MRCA prior is a valid MRCA prior.
#"
#' Calls \code{stop} if the MRCA prior is invalid.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_mrca_prior} to create a valid MRCA prior
#' @examples
#'   fasta_filename <- get_beautier_path("anthus_aco.fas")
#'   mrca_prior <- create_mrca_prior(
#'     alignment_id = get_alignment_id(fasta_filename = fasta_filename),
#'     taxa_names = get_taxa_names(filename = fasta_filename)
#'   )
#'   mrca_prior <- create_mrca_prior(
#'    alignment_id = get_alignment_id(fasta_filename = fasta_filename),
#'    taxa_names = get_taxa_names(filename = fasta_filename)
#'   )
#'   expect_silent(check_mrca_prior(mrca_prior))
#'
#'   # NA is a valid MRCA prior
#'   testthat::expect_silent(check_mrca_prior(mrca_prior = NA))
#'
#'   # Must stop on non-MRCA priors
#'   testthat::expect_error(check_mrca_prior(mrca_prior = "nonsense"))
#'   testthat::expect_error(check_mrca_prior(mrca_prior = NULL))
#' @author Richel J.C. Bilderbeek
#' @export
check_mrca_prior <- function(mrca_prior) {
  if (is_mrca_prior(mrca_prior)) { # nolint beautier function
    return()
  }
  stop(
    "'mrca_prior' must be a valid MRCA prior.\n",
    "Actual value: ", mrca_prior
  )
}
