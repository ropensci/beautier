#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param fasta_filenames the FASTA filenames
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  fasta_filenames,
  tree_priors
) {
  ids <- beastscriptr::get_file_base_sans_ext(fasta_filenames)

  text <- NULL

  text <- c(text, paste0("    <logger id=\"tracelog\" fileName=\"",
    ids, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">"))
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, paste0("        <log idref=\"treeLikelihood.",
    ids, "\"/>"))
  text <- c(text, paste0("        <log id=\"TreeHeight.t:", ids,
    "\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:",
    ids, "\"/>"))

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("        <log idref=\"BirthDeath.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDBirthRate.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDDeathRate.t:",
      ids, "\"/>"))
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0("        <parameter idref=\"popSize.t:",
      ids, "\" name=\"log\"/>"))
    text <- c(text, paste0("        <log idref=\"CoalescentConstant.t:",
      ids, "\"/>"))
  }

  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, "    <logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, paste0("        <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"))
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, paste0("    <logger id=\"treelog.t:", ids,
    "\" fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">",
    sep = "")
    )
  text <- c(text, paste0("        <log id=\"TreeWithMetaDataLogger.t:",
    ids, "\" spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
    "tree=\"@Tree.t:", ids, "\"/>",
    sep = ""))
  text <- c(text, "    </logger>")
  text
}
