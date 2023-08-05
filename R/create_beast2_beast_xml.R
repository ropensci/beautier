#' Create the \code{<beast ...>} XML
#'
#' The \code{<beast ...>} XML is the XML at the start of a BEAST2
#' XML input file, directly after the general XML declaration (as
#' created by \link{create_xml_declaration}).
#' @inheritParams default_params_doc
#' @return the XML
#' @author Richèl J.C. Bilderbeek
#' @examples
#' remove_beautier_folder()
#'
#' create_beast2_beast_xml(
#'   beauti_options = create_beauti_options_v2_6()
#' )
#'
#' check_empty_beautier_folder()
#' @export
create_beast2_beast_xml <- function(
  beauti_options
) {
  paste0(
    "<beast beautitemplate='Standard' ",
    "beautistatus='", beauti_options$status, "' ",
    "namespace=\"", beauti_options$namespace, "\" ",
    "required=\"", beauti_options$required, "\" ",
    "version=\"", beauti_options$beast2_version, "\">"
  )
}
