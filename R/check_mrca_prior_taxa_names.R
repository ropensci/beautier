#' Check the MRCA prior's taxon names are valid.
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return No return value, called for side effects
#' @export
check_mrca_prior_taxa_names <- function(taxa_names) {
  if (is_one_na(taxa_names)) return()
  if (length(taxa_names) == 0 ||
      !is.character(taxa_names) ||
      sum(taxa_names == "") > 0
  ) {
    stop("'taxa_names' must be NA or have at least one taxon name")
  }
  if (length(unique(taxa_names)) != length(taxa_names)
  ) {
    stop("'taxa_names' must be NA or all names must be unique")
  }

  invisible(taxa_names)
}
