#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @author Richel J.C. Bilderbeek
mrca_priors_to_xml_prior_distr <- function( # nolint internal function
  mrca_priors,
  has_non_strict_clock_model
) {
  testit::assert(are_mrca_priors(mrca_priors))
  if (length(mrca_priors) == 1 && is.na(mrca_priors)) return(NULL)

  text <- NULL
  taxa_names_with_ids <- NULL
  is_first <- TRUE
  for (mrca_prior in mrca_priors) {
    text <- c(
      text,
      mrca_prior_to_xml_prior_distr(
        mrca_prior,
        has_non_strict_clock_model = has_non_strict_clock_model,
        taxa_names_with_ids = taxa_names_with_ids,
        is_first = is_first
      )
    )
    taxa_names_with_ids <- unique(
      c(taxa_names_with_ids, mrca_prior$taxa_names)
    )
    is_first <- FALSE
  }
  text
}
