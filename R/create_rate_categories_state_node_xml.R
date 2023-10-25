#' Internal function
#'
#' Creates the \code{rateCategories} state node,
#' such as:
#' \code{
#'   "<stateNode id=\"rateCategories.c:[id]\"
#'     spec=\"parameter.IntegerParameter\"
#'     dimension=\"[dimension]\">
#'   1
#'   </stateNode>"
#' }
#' @inheritParams default_params_doc
#' @return the following XML:
#' \code{
#'   "<stateNode id=\"rateCategories.c:[id]\"
#'   spec=\"parameter.IntegerParameter\" dimension=\"[dimension]\">
#'   1
#'   </stateNode>"
#' }
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' create_rate_categories_state_node_xml(
#'   create_inference_model(
#'     clock_model = create_rln_clock_model(
#'       id = 314,
#'       dimension = 1
#'     )
#'   )
#' )
#'
#' check_empty_beautier_folder()
#' @export
create_rate_categories_state_node_xml <- function(inference_model) { # nolint indeed a long function name
  check_inference_model(inference_model)
  clock_model <- inference_model$clock_model
  check_true(is_rln_clock_model(clock_model))
  id <- clock_model$id
  dimension <- clock_model$dimension
  check_false(is_one_na(dimension))
  check_false(is_one_na(id))
  # dimension d = 2n - 2, where n is the number of taxa
  paste0(
    "<stateNode id=\"rateCategories.c:", id, "\" ",
    "spec=\"parameter.IntegerParameter\" ",
    "dimension=\"", dimension, "\">",
    "1", # value is always 1
    "</stateNode>"
  )
}
