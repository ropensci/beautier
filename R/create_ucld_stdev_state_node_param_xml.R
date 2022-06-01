#' Internal function
#'
#' Creates the \code{ucldStdev} parameter with the name \code{stateNode},
#' such as:
#' \code{
#'   <parameter id="ucldStdev.c:[id]" [...] name="stateNode">0.1</parameter>
#' }
#' @inheritParams default_params_doc
#' @return the following XML:
#' \code{
#'   <parameter id="ucldStdev.c:[id]" lower="0.0" name="stateNode">
#'     0.1
#'   </parameter>
#' }
#' @examples
#' check_empty_beautier_folder()
#'
#' create_ucld_stdev_state_node_param_xml(
#'   create_inference_model(
#'     clock_model = create_rln_clock_model(id = 314)
#'   )
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_ucld_stdev_state_node_param_xml <- function(inference_model) { # nolint indeed a long function name
  beautier::check_inference_model(inference_model)
  clock_model <- inference_model$clock_model
  id <- clock_model$id
  testthat::expect_true(beautier::is_rln_clock_model(clock_model))
  testthat::expect_false(beautier::is_one_na(id))
  xml <- paste0("<parameter id=\"ucldStdev.c:", id, "\" ")
  if (inference_model$beauti_options$beast2_version == "2.6") {
    xml <- paste0(xml, "spec=\"parameter.RealParameter\" ")

  }
  xml <- paste0(
    xml,
    "lower=\"0.0\" ",
    "name=\"stateNode\">",
    "0.1", # always set to 0.1 in BEAUti, cannot set it to other value
    "</parameter>"
  )
  xml
}
