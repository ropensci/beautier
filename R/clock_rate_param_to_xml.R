#' Internal function
#'
#' Converts a \code{clockRate} parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
clock_rate_param_to_xml <- function(
  clock_rate_param,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_clock_rate_param(clock_rate_param))
  id <- clock_rate_param$id
  testit::assert(beautier::is_id(id))

  xml <-  paste0(
    "<parameter ",
    "id=\"clockRate.c:", id, "\" "
  )
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(xml, "spec=\"parameter.RealParameter\" ")
  }
  xml <- paste0(
    xml,
    paste0(
      "estimate=\"", stringr::str_to_lower(clock_rate_param$estimate), "\" ",
      "name=\"clock.rate\">",
      clock_rate_param$value,
      "</parameter>"
    )
  )
  xml
}
