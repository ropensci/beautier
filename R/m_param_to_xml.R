#' Internal function
#'
#' Converts an m parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
m_param_to_xml <- function(
  m_param,
  beauti_options = create_beauti_options()
) {
  testthat::expect_true(beautier::is_m_param(m_param))
  beautier::check_beauti_options(beauti_options)
  testthat::expect_true(beautier::is_id(m_param$id))
  xml <- paste0(
    "<parameter ",
    "id=\"RealParameter.", m_param$id, "\" "
  )
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(xml, "spec=\"parameter.RealParameter\" ")
  }
  xml <- paste0(
    xml,
    "estimate=\"", stringr::str_to_lower(m_param$estimate), "\" "
  )
  if (!is.na(m_param$lower)) {
    xml <- paste0(xml, "lower=\"", m_param$lower, "\" ")
  }
  xml <- paste0(xml, "name=\"M\"")
  if (!is.na(m_param$upper)) {
    xml <- paste0(xml, " upper=\"", m_param$upper, "\"")
  }
  xml <- paste0(xml, ">", m_param$value, "</parameter>")
  xml
}

parameter_to_xml_m <- function(...) {
  stop("'parameter_to_xml_m' is deprecated, use 'm_param_to_xml' instead")
}
