#' Create the \code{<beast ...>} XML
#'
#' The \code{<beast ...>} XML is the XML at the start of a BEAST2
#' XML input file, directly after the general XML declaration (as
#' created by \link{create_xml_declaration}).
#' @inheritParams default_params_doc
#' @return the XML
#' @author Richèl J.C. Bilderbeek
#' @examples
#' create_beast2_beast_xml(
#'   beauti_options = create_beauti_options_v2_6()
#' )
#' @export
create_beast2_beast_xml <- function(
  beauti_options
) {
  paste0(
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
    "beast.evolution.operators:beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:",
    "beast.evolution.likelihood\" ",
    "required=\"", beauti_options$required, "\" ",
    "version=\"", beauti_options$beast2_version, "\">"
  )
}
