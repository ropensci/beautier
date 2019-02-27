#' Check if the MRCA prior is a valid MRCA prior.
#'
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
#'   testthat::expect_silent(check_mrca_prior(mrca_prior))
#'
#'   # NA is a valid MRCA prior
#'   testthat::expect_silent(check_mrca_prior(mrca_prior = NA))
#'
#'   # Must stop on non-MRCA priors
#'   testthat::expect_error(check_mrca_prior(mrca_prior = "nonsense"))
#'   testthat::expect_error(check_mrca_prior(mrca_prior = NULL))
#' @author Richèl J.C. Bilderbeek
#' @export
check_mrca_prior <- function(mrca_prior) {

  # An MRCA prior can be NA
  if (is_one_na(mrca_prior)) return() # nolint beautier function

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
  if (length(mrca_prior$name) != 1 ||
      (!is.character(mrca_prior$name) && !is_one_na(mrca_prior$name))) { # nolint beautier function
    stop("'name' must be NA or characters")
  }
  if (!is_one_na(mrca_prior$alignment_id) && # nolint beautier function
      !is.character(mrca_prior$alignment_id)) {
    stop("'alignment_id' must be NA or characters")
  }
  if (!is_one_na(mrca_prior$taxa_names) && # nolint beautier function
      !is.vector(mrca_prior$taxa_names, mode = "character")) {
    stop("'taxa_names' must a character vector")
  }
  if (!is.logical(mrca_prior$is_monophyletic)) {
    stop("'is_monophyletic' must be either TRUE or FALSE")
  }
  if (!is_distr(mrca_prior$mrca_distr) && # nolint beautier function
      !is_one_na(mrca_prior$mrca_distr)) { # nolint beautier function
    stop("'mrca_distr' must a distribution, as created by 'create_distr'")
  }
  testit::assert(length(mrca_prior$taxa_names) > 0)
  if (!is_one_na(mrca_prior$taxa_names) && # nolint beautier function
      sum(mrca_prior$taxa_names == "") > 0) {
    stop("'taxa_names' must be NA or have at least one taxon name")
  }
  if (!is_one_na(mrca_prior$taxa_names) && # nolint beautier function
      length(unique(mrca_prior$taxa_names)) != length(mrca_prior$taxa_names)
  ) {
    stop("'taxa_names' must be NA or all names must be unique")
  }
  if (!is_one_na(mrca_prior$clock_prior_distr_id) && # nolint beautier function
      (length(mrca_prior$clock_prior_distr_id) != 1 ||
      !is.numeric(mrca_prior$clock_prior_distr_id))) {
    stop("'clock_prior_distr_id' must be one NA or one number")
  }

}
