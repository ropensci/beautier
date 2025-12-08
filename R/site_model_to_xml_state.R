#' Internal function to convert a site model to XML,
#'   used in the `state` section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
site_model_to_xml_state <- function(
  site_model,
  beauti_options = beautier::create_beauti_options()
) {
  beautier::check_site_model(site_model)
  beautier::check_beauti_options(beauti_options)
  if (beautier::is_gtr_site_model(site_model)) {
    beautier::gtr_site_model_to_xml_state(
      site_model = site_model,
      beauti_options = beauti_options
    )
  } else if (beautier::is_hky_site_model(site_model)) {
    beautier::hky_site_model_to_xml_state(
      site_model = site_model,
      beauti_options = beauti_options
    )
  } else if (beautier::is_tn93_site_model(site_model)) {
    beautier::tn93_site_model_to_xml_state(
      site_model = site_model,
      beauti_options = beauti_options
    )
  } else {
    beautier::check_true(beautier::is_jc69_site_model(site_model))
    beautier::jc69_site_model_to_xml_state(
      site_model = site_model,
      beauti_options = beauti_options
    )
  }
}
