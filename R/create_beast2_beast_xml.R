#' Create the \code{<beast ...>} XML
#' @param beast2_version BEAST2 version
#' @param required requirement
#' @return the XML
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_beast_xml <- function(
  beast2_version,
  required = ""
) {
  if (beast2_version == "2.5") {
    paste0(
      "<beast beautitemplate='Standard' beautistatus='' ",
      "namespace=\"beast.core:beast.evolution.alignment:",
      "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
      "beast.evolution.operators:beast.evolution.sitemodel:",
      "beast.evolution.substitutionmodel:",
      "beast.evolution.likelihood\" ",
      "required=\"", required, "\" version=\"2.5\">"
    )
  } else {
    paste0(
      "<beast beautitemplate='Standard' beautistatus='' ",
      "namespace=\"beast.core:beast.evolution.alignment:",
      "beast.evolution.tree.coalescent:beast.core.util:",
      "beast.evolution.nuc:beast.evolution.operators:",
      "beast.evolution.sitemodel:",
      "beast.evolution.substitutionmodel:",
      "beast.evolution.likelihood\" ",
      "required=\"", required, "\" version=\"2.4\">"
    )
  }
}
