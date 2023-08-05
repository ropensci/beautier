#' Internal function
#'
#' Converts a `bPopSizes` parameter to XML
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
b_pop_sizes_param_to_xml <- function(
  b_pop_sizes_param,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_b_pop_sizes_param(b_pop_sizes_param))
  id <- b_pop_sizes_param$id
  testit::assert(beautier::is_id(id))

  xml <- paste0(
    "<parameter id=\"bPopSizes.t:", id, "\" ",
    "dimension=\"5\" lower=\"0.0\" name=\"stateNode\" ",
    "upper=\"380000.0\">380.0</parameter>"
  )
  xml
}
