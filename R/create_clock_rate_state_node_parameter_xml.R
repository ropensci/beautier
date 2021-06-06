#' Internal function
#'
#' Creates the \code{clockRate} parameter with the name \code{stateNode},
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
#' @author Richèl J.C. Bilderbeek
#' @examples
#' create_ucld_stdev_state_node_param_xml(
#'   create_inference_model(
#'     clock_model = create_rln_clock_model(id = 314)
#'   )
#' )
#' @export
create_clock_rate_state_node_parameter_xml <- function(inference_model) { # nolint indeed a long function name
  testthat::expect_true(
    beautier::has_strict_clock_model(inference_model) ||
    beautier::has_tip_dating(inference_model)
  )
  testthat::expect_true(beautier::has_tip_dating(inference_model))
  clock_model <- inference_model$clock_model
  id <- clock_model$id
  testthat::expect_false(beautier::is_one_na(id))

  xml <- paste0(
    "<parameter id=\"clockRate.c:", id, "\" "
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    xml <- paste0(
      xml,
      "spec=\"parameter.RealParameter\" "
    )
  }
  xml <- paste0(
    xml,
    "name=\"stateNode\">", clock_model$clock_rate_param$value,
    "</parameter>"
  )
  xml
}
