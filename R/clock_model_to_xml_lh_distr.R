#' Converts a clock model to the \code{branchRateModel} section of the
#' XML as text.
#'
#' This function will be called only if there are no MRCA priors.
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
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
clock_model_to_xml_lh_distr <- function(
  inference_model,
  clock_model = "deprecated",
  mrca_priors = "deprecated",
  tipdates_filename  = "deprecated"
) {
  if (clock_model != "deprecated") {
    stop("'clock_model' is deprecated, use 'inference_model' instead")
  }
  if (mrca_priors != "deprecated") {
    stop("'mrca_priors' is deprecated, use 'inference_model' instead")
  }
  if (tipdates_filename != "deprecated") {
    stop("'tipdates_filename' is deprecated, use 'inference_model' instead")
  }
  warning(
    "'clock_model_to_xml_lh_distr' is deprecated, ",
    "use 'create_branch_rate_model_xml' instead"
  )
  beautier::create_branch_rate_model_xml(inference_model)
}
