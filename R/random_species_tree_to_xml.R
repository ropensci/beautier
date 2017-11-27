#' Creates the XML of a random species tree,
#'   as used in the \code{init} section
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
#' @export
random_species_tree_to_xml <- function(
  id
) {
  testit::assert(is_id(id))
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
