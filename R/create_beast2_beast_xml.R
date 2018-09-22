#' Create the \code{<beast ...>} XML
create_beast2_beast_xml <- function(beast2_version) {
  if (beast2_version == "2.5") {
    paste0(
      "<beast beautitemplate='Standard' beautistatus='' ",
      "namespace=\"beast.core:beast.evolution.alignment:",
      "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
      "beast.evolution.operators:beast.evolution.sitemodel:",
      "beast.evolution.substitutionmodel:",
      "beast.evolution.likelihood\" ",
      "required=\"BEAST v2.5.0\" version=\"2.5\">"
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
      "required=\"\" version=\"2.4\">"
    )
  }
}
