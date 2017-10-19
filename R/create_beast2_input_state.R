#' Creates the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using 'get_file_base_sans_ext')
#' @param site_models one or more site models, as returned
#'   by 'create_site_model'
#' @param tree_priors one or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_state <- function(
  ids,
  site_models = create_site_model(name = "JC69"),
  tree_priors = create_tree_prior(name = "yule"),
  initial_phylogeny
) {
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

  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"birthRate.t:", ids, "\" name=\"stateNode\">1.0</parameter>"))
  } else if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"BDBirthRate.t:", ids, "\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"BDDeathRate.t:", ids, "\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"popSize.t:", ids, "\" name=\"stateNode\">0.3</parameter>"))
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"bPopSizes.t:", ids, "\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>"))
    text <- c(text, paste0("        <stateNode id=\"bGroupSizes.t:", ids, "\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>"))
  }
  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("        <parameter id=\"kappa.s:", ids, "\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"freqParameter.s:", ids, "\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>"))
  }

  text <- c(text, "    </state>")
  text
}
