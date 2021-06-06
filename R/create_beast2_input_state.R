#' Creates the '\code{state}' section of a BEAST2 XML parameter file
#'
#' Creates the '\code{state}' section of a BEAST2 XML parameter file,
#' without being indented.
#'
#' The \code{state} tag has these elements:
#' \preformatted{
#'    <state[...]>
#'        <tree[...]>
#'        [...]
#'        </tree>
#'        [parameters]
#'     </run>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{create_beast2_input_state}
#' to create the XML text of the \code{tree} tag.
#' to create the XML text of the \code{[parameters]} section.
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_state <- function(
  inference_model
) {
  text <- NULL
  # The 'tree' XML section
  text <- c(
    text,
    beautier::taxa_to_xml_tree(
      inference_model = inference_model
    )
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- c(text, "            ")
  }

  # The 'parameter' XML sections
  text <- c(
    text,
    beautier::site_model_to_xml_state(inference_model$site_model)
  )
  text <- c(
    text,
    beautier::clock_model_to_xml_state(
      inference_model = inference_model
    )
  )
  text <- c(
    text,
    beautier::tree_prior_to_xml_state(inference_model)
  )
  text <- c(
    text,
    beautier::mrca_prior_to_xml_state(
      inference_model = inference_model
    )
  )

  text <- beautier::indent(text)

  state_start_tag <- "<state id=\"state\" "
  if (inference_model$beauti_options$beast2_version == "2.6") {
    state_start_tag <- paste0(state_start_tag, "spec=\"State\" ")
  }
  state_start_tag <- paste0(state_start_tag, "storeEvery=\"5000\">")

  text <- c(state_start_tag, text)
  text <- c(text, "</state>")
  text
}
