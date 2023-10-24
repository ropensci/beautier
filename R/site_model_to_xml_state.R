#' Internal function to convert a site model to XML,
#'   used in the `state` section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
site_model_to_xml_state <- function(
  site_model,
  beauti_options = create_beauti_options()
) {
  check_site_model(site_model)
  check_beauti_options(beauti_options)
  if (is_gtr_site_model(site_model)) {
    return(
      gtr_site_model_to_xml_state(
        site_model = site_model,
        beauti_options = beauti_options
      )
    )
  } else if (is_hky_site_model(site_model)) {
    return(
      hky_site_model_to_xml_state(
        site_model = site_model,
        beauti_options = beauti_options
      )
    )
  } else if (is_tn93_site_model(site_model)) {
    return(
      tn93_site_model_to_xml_state(
        site_model = site_model,
        beauti_options = beauti_options
      )
    )
  } else {
    check_true(is_jc69_site_model(site_model))
    return(
      jc69_site_model_to_xml_state(
        site_model = site_model,
        beauti_options = beauti_options
      )
    )
  }
}
