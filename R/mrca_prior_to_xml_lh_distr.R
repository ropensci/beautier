#' Converts an MRCA prior to the \code{branchRateModel} section of the
#' XML as text.
#'
#' This function will be called if and only if there are MRCA priors
#' and only supports strict clocks at the moment.
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
#' @export
mrca_prior_to_xml_lh_distr <- function(
  inference_model,
  mrca_prior = "deprecated",
  has_non_strict_clock_model = "deprecated"
) {
  if (mrca_prior != "deprecated") {
    stop("'mrca_prior' is deprecated, use 'inference_model' instead")
  }
  if (has_non_strict_clock_model != "deprecated") {
    stop(
      "'has_non_strict_clock_model' is deprecated, ",
      "use 'inference_model' instead"
    )
  }
  warning(
    "'mrca_prior_to_xml_lh_distr' is deprecated. ",
    "Use 'create_branch_rate_model_stuff_xml' instead'"
  )
  beautier::create_branch_rate_model_stuff_xml(inference_model)
}
