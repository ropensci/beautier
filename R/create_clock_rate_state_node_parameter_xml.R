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
#' check_empty_beautier_folder()
#'
#' create_ucld_stdev_state_node_param_xml(
#'   create_inference_model(
#'     clock_model = create_rln_clock_model(id = 314)
#'   )
#' )
#'
#' check_empty_beautier_folder()
#' @export
create_clock_rate_state_node_parameter_xml <- function(inference_model) { # nolint indeed a long function name
  check_true(
    has_strict_clock_model(inference_model) ||
      has_tip_dating(inference_model)
  )
  check_true(has_tip_dating(inference_model))
  clock_model <- inference_model$clock_model
  id <- clock_model$id
  check_false(is_one_na(id))

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
