#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using 'get_file_base_sans_ext')
#' @param site_models one or more site models,
#'   as returned by 'create_site_model'
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_site_model(name = "JC69"),
  tree_priors = create_tree_prior(name = "yule")
) {

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

  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"YuleModel.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"birthRate.t:",
      ids, "\"/>"))
  } else if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"BirthDeath.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDBirthRate.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDDeathRate.t:",
      ids, "\"/>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"popSize.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"CoalescentConstant.t:",
      ids, "\"/>"))
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"BayesianSkyline.t:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"bPopSizes.t:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"bGroupSizes.t:", ids, "\"/>"))
  }

  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("        <log idref=\"kappa.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"freqParameter.s:", ids, "\"/>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0("        <log idref=\"kappa1.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"kappa2.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"freqParameter.s:", ids, "\"/>"))
  } else if (is_gtr_site_model(site_models)) {
    text <- c(text, paste0("        <log idref=\"rateAC.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateAG.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateAT.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateCG.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateGT.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"freqParameter.s:", ids, "\"/>"))

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
