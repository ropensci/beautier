#' Internal function
#'
#' Converts a `freq` parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @examples
#' check_empty_beautier_folder()
#'
#' # The freq parameter must be initialized, i.e. have an ID
#' freq_parameter_to_xml(freq_parameter = create_freq_param(id = "1"))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
freq_parameter_to_xml <- function(
  freq_parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_freq_param(freq_parameter))
  id <- freq_parameter$id
  testit::assert(beautier::is_id(id))

  xml <- paste0("<parameter id=\"freqParameter.s:", id, "\" ")
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(
      xml,
      "spec=\"parameter.RealParameter\" "
    )
  }
  xml <- paste0(
    xml,
    "dimension=\"4\" ",
    "lower=\"", freq_parameter$lower, "\" ",
    "name=\"stateNode\" upper=\"", freq_parameter$upper, "\">", freq_parameter$value, "</parameter>"
  )
  xml
}
