#' Convert a CCP tree prior
#' to the XML as part of the \code{state} section
#' @inheritParams default_params_doc
#' @return XML as text
#' @examples
#' # Need an ID and inital value
#' inference_model <- create_inference_model(
#'   tree_prior = create_ccp_tree_prior(
#'     id = "anthus_nd2_sub",
#'     pop_size_distr = create_normal_distr(
#'       id = 123,
#'       value = 3.14
#'     )
#'   )
#' )
#'
#' ccp_tree_prior_to_xml_state(inference_model)
#' @export
ccp_tree_prior_to_xml_state <- function(
  inference_model
) {
  # Do not be smart yet
  tree_prior <- inference_model$tree_prior
  testit::assert(beautier::is_id(tree_prior$id))

  parameter_xml <- paste0(
    "<parameter id=\"popSize.t:", tree_prior$id, "\" "
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    parameter_xml <- paste0(
      parameter_xml, "spec=\"parameter.RealParameter\" "
    )
  }
  if (!is.na(tree_prior$pop_size_distr$lower)) {
    parameter_xml <- paste0(
      parameter_xml,
      paste0("lower=\"", tree_prior$pop_size_distr$lower, "\" ")
    )
  }
  parameter_xml <- paste0(
    parameter_xml,
    "name=\"stateNode\""
  )
  if (!is.na(tree_prior$pop_size_distr$upper)) {
    parameter_xml <- paste0(
      parameter_xml,
      " upper=\"", tree_prior$pop_size_distr$upper, "\""
    )
  }
  parameter_xml <- paste0(
    parameter_xml,
    ">",
    tree_prior$pop_size_distr$value,
    "</parameter>"
  )
  parameter_xml
}
