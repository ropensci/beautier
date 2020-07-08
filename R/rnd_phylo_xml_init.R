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

  init_xml_begin <- paste0("<init id=\"RandomTree.t:", id,
    "\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\"",
    " initial=\"@Tree.t:", id, "\" taxa=\"@", id, "\">"
  )

  population_model_xml_begin <- paste0(
    "<populationModel id=\"ConstantPopulation0.t:",
    id, "\" spec=\"ConstantPopulation\">"
  )
  parameter_xml <- paste0(
    "<parameter id=\"randomPopSize.t:", id, "\" "
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    parameter_xml <- paste0(parameter_xml, "spec=\"parameter.RealParameter\" ")
  }
  parameter_xml <- paste0(parameter_xml, "name=\"popSize\">1.0</parameter>")

  population_model_xml_end <- "</populationModel>"
  init_xml_end <- "</init>"

  c(
    init_xml_begin,
    paste0("    ", population_model_xml_begin),
    paste0("        ", parameter_xml),
    paste0("    ", population_model_xml_end),
    init_xml_end
  )
}
