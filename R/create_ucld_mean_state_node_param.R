#' Internal function
#'
#' Creates the \code{ucldMean.c} parameter with the name \code{stateNode},
#' such as:
#' \code{<parameter id=\"ucldMean.c:[id]\" name=\"stateNode\">1.0</parameter>}
#' @inheritParams default_params_doc
#' @return the XML
#' \code{<parameter id=\"ucldMean.c:[id]\" name=\"stateNode\">1.0</parameter>}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # "<parameter id=\"ucldMean.c:314\" name=\"stateNode\">1.0</parameter>"
#' create_ucld_mean_state_node_param(
#'   create_inference_model(
#'     clock_model = create_rln_clock_model(id = 314)
#'   )
#' )
#' @export
create_ucld_mean_state_node_param <- function(inference_model) { # nolint indeed a long function name
  beautier::check_inference_model(inference_model)
  clock_model <- inference_model$clock_model
  id <- clock_model$id
  mean_clock_rate <- clock_model$mean_clock_rate
  testthat::expect_true(beautier::is_rln_clock_model(clock_model))
  testthat::expect_false(beautier::is_one_na(id))
  testthat::expect_false(beautier::is_one_na(mean_clock_rate))
  paste0(
    "<parameter id=\"ucldMean.c:", id, "\" ",
    "name=\"stateNode\">",
    mean_clock_rate,
    "</parameter>"
  )
}
