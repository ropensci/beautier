#' Create a Most Recent Common Ancestor prior
#' @inheritParams default_params_doc
#' @param taxa_names names of the taxa,
#'   as returned by \code{\link{get_taxa_names}}
#' @param mrca_distr the MRCA distribution,
#'   as returned by \code{\link{create_distr}}
#' @return an MRCA prior
#' @author Richel J.C. Bilderbeek
#' @export
create_mrca_prior <- function(
  alignment_id,
  taxa_names,
  mrca_distr
) {
  if (!is.character(alignment_id)) {
    stop("'alignment_id' must be characters")
  }
  if (!is.vector(taxa_names, mode = "character")) {
    stop("'taxa_names' must a character vector")
  }
  if (!is_distr(mrca_distr)) {
    stop("'mrca_distr' must a distribution, as created by 'create_distr'")
  }
  mrca_prior <- list(
    alignment_id = alignment_id,
    taxa_names = taxa_names,
    mrca_distr = mrca_distr
  )
  testit::assert(is_mrca_prior(mrca_prior))
  mrca_prior
}
