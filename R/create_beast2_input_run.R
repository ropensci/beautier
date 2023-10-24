#' Creates the '\code{run}' section of a BEAST2 XML parameter file
#'
#' Creates the '\code{run}' section of a BEAST2 XML parameter file,
#' without being indented.
#'
#' The \code{run} tag has these elements:
#' \preformatted{
#'    <run[...]>
#'        <state[...]>
#'        [...]
#'        </state>
#'        <init[...]>
#'        [...]
#'        </init>
#'        <distribution[...]>
#'        [...]
#'        </distribution>
#'        [operator ids]
#'        [loggers]
#'     </run>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{create_beast2_input_state}
#' to create the XML text of the \code{state} tag.
#' Use \link{create_beast2_input_init}
#' to create the XML text of the \code{init} tag.
#' Use \link{create_beast2_input_distr}
#' to create the XML text of the \code{distribution} tag.
#' Use \link{create_beast2_input_operators}
#' to create the XML text of the \code{[operator ids]} section.
#' Use \link{create_loggers_xml}
#' to create the XML text of the \code{[loggers]} part.
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_run <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  check_true(length(input_filename) == 1)

  # Create the '<run...' starting tag
  text <- mcmc_to_xml_run(inference_model$mcmc)
  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- c(text, "        ")
  }

  # Create the '<state...' part
  text <- c(text,
    indent(
      create_beast2_input_state(
        inference_model = inference_model
      )
    )
  )

  text <- c(text,
    create_beast2_input_init(
      inference_model = inference_model
    )
  )

  text <- c(text, "")

  text <- c(text,
    indent(
      create_beast2_input_distr(
        inference_model = inference_model
      )
    )
  )

  text <- c(text, "")

  text <- c(
    text,
    indent(
      create_beast2_input_operators(
        inference_model = inference_model
      )
    )
  )

  text <- c(text, "")

  text <- c(
    text,
    create_loggers_xml(
      input_filename = input_filename,
      inference_model = inference_model
    )
  )

  text <- c(text, "")

  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- c(text, "    <operatorschedule id=\"OperatorSchedule\" spec=\"OperatorSchedule\"/>") # nolint
    text <- c(text, "")
  }

  text <- c(text, "</run>")
  text
}
