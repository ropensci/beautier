#' Internal function
#'
#' Converts a Bayesian population sizes parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
b_pop_sizes_parameter_to_xml <- function(
  b_pop_sizes_parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  # Don't be smart yet
  parameter <- b_pop_sizes_parameter
  testit::assert(beautier::is_b_pop_sizes_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  xml <- paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" "
  )
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(
      xml,
      "spec=\"parameter.RealParameter\" "
    )
  }
  xml <- paste0(
    xml,
    "estimate=\"", estimate, "\" ",
    "name=\"beta\">", parameter$value,
    "</parameter>"
  )
  xml
}
