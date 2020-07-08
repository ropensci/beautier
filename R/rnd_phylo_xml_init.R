#' Creates the XML of a random phylogeny,
#'   as used in the \code{init} section
#'
#' Creates the XML text for the \code{beast} tag of a BEAST2 parameter file,
#' which is directly after the XML
#' declaration (created by \link{create_xml_declaration}.
#'
#' The \code{init} tag has these elements:
#'
#' \preformatted{
#'   <init id=\"RandomTree.t:[...]>
#'       <populationModel[...]>
#'       [...]
#'       </populationModel>
#'   </init>
#' }
#' @inheritParams default_params_doc
#' @return the phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
rnd_phylo_to_xml_init <- function(
  inference_model,
  id = "deprecated"
) {
  if (id != "deprecated") {
    stop("'id' is deprecated, use 'inference_model' instead")
  }

  id <- inference_model$site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  text <- c(text, paste0("<init id=\"RandomTree.t:", id,
    "\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\"",
    " initial=\"@Tree.t:", id, "\" taxa=\"@", id, "\">"
  ))
  text <- c(text, paste0(
    "    <populationModel id=\"ConstantPopulation0.t:",
    id, "\" spec=\"ConstantPopulation\">"))
  text <- c(text, paste0("        <parameter id=\"randomPopSize.t:",
    id, "\" name=\"popSize\">1.0</parameter>"))
  text <- c(text, "    </populationModel>")
  text <- c(text, "</init>")
  text
}
