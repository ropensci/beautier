#' Internal function
#'
#' Converts an kappa parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @examples
#' check_empty_beautier_folder()
#'
#' # The kappa parameter must be initialized, i.e. have an ID
#' kappa_param_to_xml(kappa_param = create_kappa_param(id = "1"))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
kappa_param_to_xml <- function(
  kappa_param,
  beauti_options = create_beauti_options()
) {
  check_beauti_options(beauti_options)
  check_true(is_kappa_param(kappa_param))
  id <- kappa_param$id
  check_true(is_id(id))

  xml <- paste0("<parameter id=\"kappa.s:", id, "\" ")
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(
      xml,
      "spec=\"parameter.RealParameter\" "
    )
  }
  xml <- paste0(
    xml,
    "lower=\"", kappa_param$lower, "\" ",
    "name=\"stateNode\">", kappa_param$value, "</parameter>"
  )
  xml
}
