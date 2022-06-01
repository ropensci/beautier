#' Internal function
#'
#' Creates the \code{ucldMean.c} parameter with the name \code{stateNode},
#' such as:
#' \code{
#'   <parameter id=\"ucldMean.c:[id]\" spec=\"parameter.RealParameter\"
#'     name=\"stateNode\">1.0</parameter>
#' }
#' @inheritParams default_params_doc
#' @return the XML
#' \code{
#'   <parameter id=\"ucldMean.c:[id]\" spec=\"parameter.RealParameter\"
#'     name=\"stateNode\">1.0</parameter>
#' }
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' create_ucld_mean_state_node_param_xml(
#'   create_inference_model(
#'     clock_model = create_rln_clock_model(id = 314),
#'     beauti_options = create_beauti_options_v2_6()
#'   )
#' )
#'
#' check_empty_beautier_folder()
#' @export
create_ucld_mean_state_node_param_xml <- function(inference_model) { # nolint indeed a long function name
  if (inference_model$beauti_options$beast2_version == "2.4") {
    stop("The ucldMean stateNode was absent in BEAST v2.4")
  }
  beautier::check_inference_model(inference_model)
  clock_model <- inference_model$clock_model
  id <- clock_model$id
  mean_clock_rate <- clock_model$mean_clock_rate
  testthat::expect_true(beautier::is_rln_clock_model(clock_model))
  testthat::expect_false(beautier::is_one_na(id))
  testthat::expect_false(beautier::is_one_na(mean_clock_rate))
  xml <- paste0(
    "<parameter id=\"ucldMean.c:", id, "\" "
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    xml <- paste0(xml, "spec=\"parameter.RealParameter\" ")
  }
  xml <- paste0(xml,
    "name=\"stateNode\">",
    mean_clock_rate,
    "</parameter>"
  )
  xml
}
