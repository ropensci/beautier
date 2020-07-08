#' Converts a site model to XML,
#'   used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
site_model_to_xml_subst_model <- function(
  inference_model,
  site_model = "deprecated"
) {
  if (site_model != "deprecated") {
    stop("'site_model' is deprecated, use 'inference_model' instead")
  }
  warning(
    "'site_model_to_xml_subst_model' is deprecated, ",
    "use 'create_subst_model_xml' instead"
  )
  beautier::create_subst_model_xml(inference_model)
}
