#' Creates the the \code{distribution}'s prior section (which is part of
#' a posterior distribution section) of a BEAST2 XML parameter file.
#'
#' These lines start with '\code{<distribution id="prior"}'
#'
#' \code{
#'    <distribution id="posterior" spec="util.CompoundDistribution">
#'        <distribution id="prior" spec="util.CompoundDistribution">
#'          HERE, where the ID of the distribution is 'prior'
#'        </distribution>
#'        <distribution id="likelihood" ...>
#'        </distribution>
#'   </distribution>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_priors_to_xml_prior_distr <- function(
  mrca_priors,
  has_non_strict_clock_model
) {
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  if (length(mrca_priors) == 1 && beautier::is_one_na(mrca_priors)) return(NULL)

  text <- NULL
  taxa_names_with_ids <- NULL
  for (mrca_prior in mrca_priors) {
    text <- c(
      text,
      mrca_prior_to_xml_prior_distr(
        mrca_prior,
        has_non_strict_clock_model = has_non_strict_clock_model,
        taxa_names_with_ids = taxa_names_with_ids
      )
    )
    testit::assert(!beautier::is_one_na(mrca_prior$taxa_names))
    taxa_names_with_ids <- unique(
      c(taxa_names_with_ids, mrca_prior$taxa_names)
    )
  }
  text
}
