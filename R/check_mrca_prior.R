#' Check if the MRCA prior is a valid MRCA prior.
#'
#' Calls \code{stop} if the MRCA prior is invalid.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_mrca_prior} to create a valid MRCA prior
#' @examples
#' check_empty_beautier_folder()
#'
#' fasta_filename <- get_beautier_path("anthus_aco.fas")
#' mrca_prior <- create_mrca_prior(
#'   alignment_id = get_alignment_id(fasta_filename = fasta_filename),
#'   taxa_names = get_taxa_names(filename = fasta_filename)
#' )
#' mrca_prior <- create_mrca_prior(
#'  alignment_id = get_alignment_id(fasta_filename = fasta_filename),
#'  taxa_names = get_taxa_names(filename = fasta_filename)
#' )
#' check_mrca_prior(mrca_prior)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_mrca_prior <- function(mrca_prior) {

  # An MRCA prior can be NA
  if (beautier::is_one_na(mrca_prior)) return()

  # If not, it should have all list elements needed
  beautier::check_mrca_prior_names(mrca_prior)

  beautier::check_is_monophyletic(mrca_prior$is_monophyletic)
  beautier::check_mrca_prior_name(mrca_prior$name)
  beautier::check_alignment_id(mrca_prior$alignment_id)
  beautier::check_mrca_prior_taxa_names(mrca_prior$taxa_names)

  if (!beautier::is_distr(mrca_prior$mrca_distr) &&
      !beautier::is_one_na(mrca_prior$mrca_distr)) {
    stop("'mrca_distr' must a distribution, as created by 'create_distr'")
  }
  if (!beautier::is_one_na(mrca_prior$clock_prior_distr_id) &&
      !beautier::is_one_int(mrca_prior$clock_prior_distr_id)
  ) {
    stop("'clock_prior_distr_id' must be one NA or one number")
  }

}

#' Check if the MRCA prior,
#' which is a list, has all the named elements.
#'
#' Calls \code{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{check_mrca_prior} to check the entire MRCA prior
#' @author Richèl J.C. Bilderbeek
#' @export
check_mrca_prior_names <- function(
  mrca_prior
) {
  argument_names <- c(
    "name", "alignment_id", "taxa_names", "is_monophyletic", "mrca_distr",
    "clock_prior_distr_id"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(mrca_prior)) {
      stop(
        "'", arg_name, "' must be an element of an 'mrca_prior'. ",
        "Tip: use 'create_mrca_prior'"
      )
    }
  }
}
