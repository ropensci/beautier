#' Create a Most Recent Common Ancestor prior
#' @inheritParams default_params_doc
#' @param taxa_names names of the taxa,
#'   as returned by \code{\link{get_taxa_names}}
#' @return an MRCA prior
#' @author Richel J.C. Bilderbeek
#' @export
create_mrca_prior <- function(
  taxa_names
) {
  list(taxa_names = taxa_names)
}
