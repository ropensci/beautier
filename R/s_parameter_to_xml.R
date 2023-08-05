#' Internal function
#'
#' Converts an `s_param` to XML
#' @inheritParams default_params_doc
#' @param parameter a s parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_s_param}})
#' @return the parameter as XML text
#' @examples
#' s_parameter_to_xml(
#'   create_s_param(id = 4, value = 1.25),
#'   beauti_options = create_beauti_options_v2_4()
#' )
#' s_parameter_to_xml(
#'   create_s_param(id = 4, value = 1.25),
#'   beauti_options = create_beauti_options_v2_6()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
s_parameter_to_xml <- function(
  parameter,
  beauti_options
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_s_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  lower <- parameter$lower
  upper <- parameter$upper
  text <- paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\""
  )
  if (beauti_options$beast2_version == "2.6") {
    text <- paste0(text, " spec=\"parameter.RealParameter\"")
  }

  text <- paste0(
    text,
    " estimate=\"", estimate, "\""
  )
  if (!beautier::is_one_na(lower)) {
    if (lower != 0.0) {
      text <- paste0(text, " lower=\"", lower, "\"")
    }
  }
  text <- paste0(text, " name=\"S\"")
  if (!beautier::is_one_na(upper)) {
    if (!is.infinite(upper)) {
      upper_txt <- upper
      if (is.infinite(upper)) {
        upper_txt <- "Infinity"
      }
      text <- paste0(text, " upper=\"", upper_txt, "\"")
    }
  }
  text <- paste0(text, ">", value, "</parameter>")
  text
}
