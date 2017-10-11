#' Creates the state section of a BEAST2 XML parameter file
#' @param filename_base filename its base
#' @param tree_priors one or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_state <- function(
  filename_base,
  tree_priors,
  initial_phylogeny
) {
  text <- NULL
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")

  if (!ribir::is_phylogeny(initial_phylogeny)) {
    text <- c(text, paste0("        <tree id=\"Tree.t:",
      filename_base, "\" name=\"stateNode\">"))
    text <- c(text, paste0("            <taxonset id=\"TaxonSet.",
      filename_base, "\" spec=\"TaxonSet\">"))
    text <- c(text, paste0("                <alignment idref=\"",
      filename_base, "\"/>"))
    text <- c(text, "            </taxonset>")
    text <- c(text, "        </tree>")
  } else {
    text <- c(text, paste0("    <stateNode spec=\"beast.util.TreeParser\" ",
        "id=\"Tree.t:", filename_base, "\" IsLabelledNewick=\"true\" ",
        "adjustTipHeights=\"false\" taxa=\"@", filename_base, "\" ",
        "newick=\"", ape::write.tree(initial_phylogeny), "\">"))
    text <- c(text, paste0("    </stateNode>"))
  }

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("        <parameter id=\"birthRate2.t:",
      filename_base,
      "\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>",
      sep = "")
    )
    text <- c(text, paste0("        <parameter id=\"relativeDeathRate2.t:",
      filename_base, "\" lower=\"0.0\" name=\"stateNode\"",
      " upper=\"1.0\">0.5</parameter>"))
  } else {
    testit::assert(tree_priors == "coalescent_constant_population")
    text <- c(text, paste0("        <parameter id=\"popSize.t:",
      filename_base, "\" name=\"stateNode\">0.3</parameter>"))
  }

  text <- c(text, "    </state>")
  text
}
