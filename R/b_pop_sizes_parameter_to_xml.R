#' Internal function
#'
#' Converts a Bayesian population sizes parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @examples
#' b_pop_sizes_parameter_to_xml(
#'   b_pop_sizes_parameter = create_b_pop_sizes_param(id = 42),
#'   beauti_options = create_beauti_options()
#' )
#' b_pop_sizes_parameter_to_xml(
#'   b_pop_sizes_parameter = create_b_pop_sizes_param(id = 42, upper = Inf),
#'   beauti_options = create_beauti_options()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
b_pop_sizes_parameter_to_xml <- function(
  b_pop_sizes_parameter,
  beauti_options = create_beauti_options()
) {
  check_beauti_options(beauti_options)
  # Don't be smart yet
  parameter <- b_pop_sizes_parameter
  check_true(is_b_pop_sizes_param(parameter))
  id <- parameter$id
  check_true(is_id(id))
  check_true("upper" %in% names(b_pop_sizes_parameter))
  xml <- paste0(
    "<parameter id=\"bPopSizes.t:", id, "\" "
  )
  if (beauti_options$beast2_version == "2.6") {
    xml <- paste0(
      xml,
      "spec=\"parameter.RealParameter\" "
    )
  }
  xml <- paste0(
    xml,
    "dimension=\"5\" lower=\"0.0\" name=\"stateNode\""
  )
  if (!is.infinite(b_pop_sizes_parameter$upper)) {
    xml <- paste0(xml, " upper=\"", b_pop_sizes_parameter$upper, "\"")
  }
  xml <- paste0(xml, ">380.0</parameter>")
  xml
}
