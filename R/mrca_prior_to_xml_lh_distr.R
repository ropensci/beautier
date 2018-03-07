#' Converts an MRCA prior to the \code{branchRateModel} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
mrca_prior_to_xml_lh_distr <- function(
  mrca_prior
) {
  testit::assert(is_mrca_prior(mrca_prior))
  if (length(mrca_prior) == 1 && is.na(mrca_prior)) return(NULL)
  paste0(
    "<branchRateModel ",
    "id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
    "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
    "clock.rate=\"@clockRate.c:", mrca_prior$alignment_id, "\"/>"
  )
}
