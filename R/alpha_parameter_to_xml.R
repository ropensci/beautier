#' Internal function
#'
#' Converts an alpha parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @examples
#' remove_beautier_folder()
#' check_empty_beautier_folder()
#'
#' # The alpha parameter must be initialized, i.e. have an ID
#' alpha_parameter_to_xml(alpha_parameter = create_alpha_param(id = "1"))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
alpha_parameter_to_xml <- function(
  alpha_parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)

  # Don't be smart yet
  parameter <- alpha_parameter

  testit::assert(beautier::is_alpha_param(parameter))
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
    "name=\"alpha\">", parameter$value,
    "</parameter>"
  )
  xml
}
