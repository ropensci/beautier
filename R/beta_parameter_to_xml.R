#' Internal function
#'
#' Converts a beta parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
beta_parameter_to_xml <- function(
  beta_parameter,
  beauti_options = create_beauti_options()
) {
  check_beauti_options(beauti_options)
  # Don't be smart yet
  parameter <- beta_parameter
  check_true(is_beta_param(parameter))
  id <- parameter$id
  check_true(is_id(id))
  check_true(parameter$estimate == FALSE)
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
