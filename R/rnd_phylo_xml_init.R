#' Creates the XML of a random phylogeny,
#'   as used in the \code{init} section
#' @inheritParams default_params_doc
#' @return the phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
rnd_phylo_to_xml_init <- function(
  id
) {
  testit::assert(is_id(id)) # nolint beautier function
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
