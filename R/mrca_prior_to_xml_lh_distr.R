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
  mrca_prior,
  has_non_strict_clock_model = FALSE
) {
  testit::assert(is_mrca_prior(mrca_prior))
  if (length(mrca_prior) == 1 && is.na(mrca_prior)) return(NULL)
  if (!has_non_strict_clock_model && mrca_prior$is_monophyletic) {
    paste0(
      "<branchRateModel ",
      "id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", mrca_prior$alignment_id, "\"/>"
    )
  } else if (!has_non_strict_clock_model) {
    testit::assert(!mrca_prior$is_monophyletic)
    text <- NULL
    text <- c(
      text,
      paste0(
        "<branchRateModel id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
        "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"
      )
    )
    text <- c(
      text,
      paste0(
        "    <parameter id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
        "estimate=\"false\" name=\"clock.rate\">1.0</parameter>"
      )
    )
    text <- c(text, paste0("</branchRateModel>"))
    text
  }
}
