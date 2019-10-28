#' Creates the three logger sections of a BEAST2 XML parameter file
#'
#' The logger section has these elements:
#' \preformatted{
#'  <logger id="tracelog" [...]>
#'      [...]
#'  </logger>
#'  <logger id="screenlog" [...]>
#'      [...]
#'  </logger>
#'  <logger id="treelog.t:[alignment ID]"  [...]>
#'      [...]
#'  </logger>
#' }
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{create_tracelog_xml} to create the XML text
#' of the logger with the \code{tracelog} ID.
#' Use \link{create_screenlog_xml} to create the XML text
#' of the logger with the \code{screenlog} ID.
#' Use \link{create_treelog_xml} to create the XML text
#' of the loggers with the \code{treelog} ID.
#' @author Richèl J.C. Bilderbeek
#' @export
create_loggers_xml <- function(
  input_filename,
  inference_model
) {
  testit::assert(length(input_filename) == 1)
  beautier::check_inference_model(inference_model)

  tracelog_text <- beautier::create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )

  screenlog_text <- beautier::create_screenlog_xml(inference_model)

  treelogs_text <- beautier::create_treelog_xml(inference_model)


  c(
    beautier::indent(tracelog_text),
    "",
    beautier::indent(screenlog_text),
    beautier::indent(treelogs_text)
  )
}
