#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param filename_base filename_base
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  filename_base,
  tree_priors
) {
  text <- NULL

  text <- c(text, paste0("    <logger id=\"tracelog\" fileName=\"",
    filename_base, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">"))
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, paste0("        <log idref=\"treeLikelihood.",
    filename_base, "\"/>"))
  text <- c(text, paste0("        <log id=\"TreeHeight.t:", filename_base,
    "\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:",
    filename_base, "\"/>"))

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("        <log idref=\"BirthDeath.t:",
      filename_base, "\"/>"))
    text <- c(text, paste0("        <log idref=\"birthRate2.t:",
      filename_base, "\"/>"))
    text <- c(text, paste0("        <log idref=\"relativeDeathRate2.t:",
      filename_base, "\"/>"))
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0("        <parameter idref=\"popSize.t:",
      filename_base, "\" name=\"log\"/>"))
    text <- c(text, paste0("        <log idref=\"CoalescentConstant.t:",
      filename_base, "\"/>"))
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
  text <- c(text, paste0("    <logger id=\"treelog.t:", filename_base,
    "\" fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">",
    sep = "")
    )
  text <- c(text, paste0("        <log id=\"TreeWithMetaDataLogger.t:",
    filename_base, "\" spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
    "tree=\"@Tree.t:", filename_base, "\"/>",
    sep = ""))
  text <- c(text, "    </logger>")
  text
}
