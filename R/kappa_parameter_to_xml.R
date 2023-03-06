#' Internal function
#'
#' Converts an kappa parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @examples
#' check_empty_beautier_folder()
#'
#' # The kappa parameter must be initialized, i.e. have an ID
#' kappa_parameter_to_xml(kappa_parameter = create_kappa_param(id = "1"))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
kappa_parameter_to_xml <- function(
  kappa_parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_kappa_param(kappa_parameter))
  id <- kappa_parameter$id
  testit::assert(beautier::is_id(id))

  xml <- paste0("<parameter id=\"kappa.s:", id, "\" ")
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(
      xml,
      "spec=\"parameter.RealParameter\" "
    )
  }
  xml <- paste0(
    xml,
    "lower=\"", kappa_parameter$lower, "\" ",
    "name=\"stateNode\">", kappa_parameter$value, "</parameter>"
  )
  xml
}
