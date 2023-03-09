#' Internal function
#'
#' Internal function to converts a clock model
#' to the \code{prior} section of the XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' clock_model_to_xml_prior_distr(
#'   inference_model = create_inference_model()
#' )
#' check_empty_beautier_folder()
#' @export
clock_model_to_xml_prior_distr <- function(
  inference_model
) {
  testit::assert(beautier::is_clock_model(inference_model$clock_model))

  if (beautier::is_rln_clock_model(inference_model$clock_model)) {
    return(beautier::rln_clock_model_to_xml_prior_distr(inference_model))
  } else {
    # Fails for unimplemented clock models
    testit::assert(beautier::is_strict_clock_model(inference_model$clock_model))
    return(beautier::strict_clock_model_to_xml_prior_distr(inference_model))
  }
}
