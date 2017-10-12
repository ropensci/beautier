#' Creates the state section of a BEAST2 XML parameter file
#' @param fasta_filenames the fasta filenames
#' @param tree_priors one or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_state <- function(
  fasta_filenames,
  tree_priors,
  initial_phylogeny
) {
  ids <- get_file_base_sans_ext(fasta_filenames)
  text <- NULL
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")

  if (!ribir::is_phylogeny(initial_phylogeny)) {
    text <- c(text, paste0("        <tree id=\"Tree.t:",
      ids, "\" name=\"stateNode\">"))
    text <- c(text, paste0("            <taxonset id=\"TaxonSet.",
      ids, "\" spec=\"TaxonSet\">"))
    text <- c(text, paste0("                <alignment idref=\"",
      ids, "\"/>"))
    text <- c(text, "            </taxonset>")
    text <- c(text, "        </tree>")
  } else {
    text <- c(text, paste0("    <stateNode spec=\"beast.util.TreeParser\" ",
        "id=\"Tree.t:", ids, "\" IsLabelledNewick=\"true\" ",
        "adjustTipHeights=\"false\" taxa=\"@", ids, "\" ",
        "newick=\"", ape::write.tree(initial_phylogeny), "\">"))
    text <- c(text, paste0("    </stateNode>"))
  }

  if (tree_priors$name == "birth_death") {

    text <- c(text, paste0("        <parameter id=\"BDBirthRate.t:", ids, "\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"BDDeathRate.t:", ids, "\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>"))

    # text <- c(text, paste0("        <parameter id=\"birthRate2.t:",
    #   ids,
    #   "\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>"
    #   )
    # )
    # text <- c(text, paste0("        <parameter id=\"relativeDeathRate2.t:",
    #   ids, "\" lower=\"0.0\" name=\"stateNode\"",
    #   " upper=\"1.0\">0.5</parameter>"))
  } else {
    testit::assert(tree_priors == "coalescent_constant_population")
    text <- c(text, paste0("        <parameter id=\"popSize.t:",
      ids, "\" name=\"stateNode\">0.3</parameter>"))
  }

  text <- c(text, "    </state>")
  text
}
